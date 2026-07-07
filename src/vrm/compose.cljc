(ns vrm.compose
  "Part composition: merge multiple VrmParts into a single VrmDocument.
  Restored from `kami-vrm/src/compose.rs` (kotoba-lang/kami-engine, deleted
  PR #82) as part of the clj-wgsl migration (ADR-2607010930,
  com-junkawasaki/root).

  `PartSource` = `{:part VrmPart :doc VrmDocument}`. `ComposeConfig` =
  `{:skeleton-base index}` (index into `sources` whose skeleton is the
  canonical armature)."
  (:require [vrm.gltf-types :as gt]
            [vrm.vrm-types :as vt]
            [vrm.convert :as conv]))

(defn- write-f32-le
  "float -> a 4-byte little-endian vector, the write-side mirror of vrm.convert's
  private `read-f32-le` (same #?(:clj/:cljs) split: `Float/floatToIntBits` +
  unchecked-int byte extraction on JVM, `DataView` on cljs)."
  [f]
  #?(:clj (let [bits (Float/floatToIntBits (float f))]
            [(bit-and bits 0xFF) (bit-and (bit-shift-right bits 8) 0xFF)
             (bit-and (bit-shift-right bits 16) 0xFF) (bit-and (bit-shift-right bits 24) 0xFF)])
     :cljs (let [buf (js/ArrayBuffer. 4) view (js/DataView. buf)]
             (.setFloat32 view 0 f true)
             [(.getUint8 view 0) (.getUint8 view 1) (.getUint8 view 2) (.getUint8 view 3)])))

(defn- find-parent-in-remap*
  "Walk up `doc`'s node tree from `node-idx` to find a mapped ancestor for
  `src-idx` in `node-remap` (`{[src-idx old-node-idx] new-node-idx}`),
  recursing through unmapped ancestors (faithful port of the Rust original's
  recursive walk)."
  [doc node-idx src-idx node-remap]
  (let [nodes (:nodes (:gltf doc))
        parent-idx (some (fn [[i n]] (when (some #{node-idx} (:children n)) i))
                          (map-indexed vector nodes))]
    (when parent-idx
      (if-let [mapped (get node-remap [src-idx parent-idx])]
        mapped
        (find-parent-in-remap* doc parent-idx src-idx node-remap)))))

(defn compose
  "Compose multiple VRM parts (`sources`, a seq of `PartSource`) into a
  single VrmDocument, per `config` (`ComposeConfig`). Phases: skeleton
  unification -> buffer merge -> mesh merge -> joint remap -> material merge
  -> spring bone merge -> expression merge -> rebuild."
  [sources config]
  (when (empty? sources) (throw (ex-info "part error: no sources provided" {})))
  (let [sources (vec sources)
        skeleton-base (:skeleton-base config)
        base-src (get sources skeleton-base)]
    (when-not base-src (throw (ex-info "part error: skeleton_base out of range" {})))
    (let [base-doc (:doc base-src)
          base-gltf (:gltf base-doc)

          ;; ── Source-document dedup (real bug fix, /loop maturity pass) ──
          ;; Multiple `PartSource`s commonly share the SAME underlying `:doc`
          ;; (e.g. picking hair+face+outfit+other all from one uploaded VRM
          ;; whose sibling lacks a `:body` part -- the character-creator app's
          ;; own auto-pick default when mixing two real VRMs). Buffer/image/
          ;; texture merging below used to key its "have I already copied this
          ;; byte range?" bookkeeping by `src-idx` (position in `sources`), not
          ;; by which DOCUMENT that source actually points at -- so a shared
          ;; document's full binary buffer (images included) got concatenated
          ;; once PER PART referencing it, not once per unique document. A
          ;; real measured case (2 real files, 5 parts, 4 sharing one
          ;; document) exported 54,228,120 bytes from just 21,318,792 bytes of
          ;; real source `:bin` data (~2.5x bloat) -- exactly `4 * seedsan-bin
          ;; + 1 * twist-bin` (53,667,900 pre-export), confirming 4x
          ;; duplication of the shared document, not some vaguer inefficiency.
          ;; `canon-idx` maps each `sources` position to the position of the
          ;; FIRST source sharing its `:doc` (via `identical?` -- the character-
          ;; creator app's `PartSource`s built from one loaded `VrmDocument`
          ;; genuinely share one object, not just structurally-equal copies,
          ;; so reference equality is both correct and cheap here, unlike `=`
          ;; on multi-megabyte maps). Every place below that keys a buffer-
          ;; view/accessor/image/texture remap table uses `(canon src-idx)`
          ;; instead of the raw `src-idx`, so a document's expensive-to-copy
          ;; data is merged exactly once regardless of how many parts of it
          ;; are used; per-part-scoped data (node/mesh reachability, which
          ;; already only touches `(:part src)`'s own small index lists, not
          ;; the whole document) stays keyed by the real `src-idx`, since two
          ;; different parts of the same document legitimately reference
          ;; different mesh/material subsets.
          canon-idx
          (vec (reduce (fn [acc [i src]]
                         (conj acc (or (some (fn [j] (when (identical? (:doc (nth sources j)) (:doc src)) j))
                                              (range i))
                                       i)))
                       []
                       (map-indexed vector sources)))
          canon (fn [src-idx] (nth canon-idx src-idx))

          ;; ── Phase 1: skeleton unification ──
          base-node-count (count (:nodes base-gltf))
          node-remap0 (into {} (for [i (range base-node-count)] [[skeleton-base i] i]))
          base-bone-map (into {} (map (fn [hb] [(:bone hb) (:node hb)])) (:human-bones (:humanoid base-doc)))

          unify-source
          (fn [[unified-nodes node-remap] src-idx src]
            (if (= src-idx skeleton-base)
              [unified-nodes node-remap]
              (let [src-doc (:doc src)
                    src-gltf (:gltf src-doc)
                    src-bone-map (into {} (map (fn [hb] [(:bone hb) (:node hb)])) (:human-bones (:humanoid src-doc)))
                    ;; Map humanoid bones to base.
                    node-remap (reduce
                                (fn [nr [bone-name src-node]]
                                  (if-let [base-node (get base-bone-map bone-name)]
                                    (assoc nr [src-idx src-node] base-node)
                                    nr))
                                node-remap
                                src-bone-map)
                    ;; Append non-humanoid nodes.
                    [unified-nodes node-remap]
                    (reduce
                     (fn [[unified-nodes node-remap] ni]
                       (if (contains? node-remap [src-idx ni])
                         [unified-nodes node-remap]
                         (let [parent-new (find-parent-in-remap* src-doc ni src-idx node-remap)
                               new-node (-> (get (:nodes src-gltf) ni)
                                            (assoc :children [] :mesh nil))
                               new-idx (count unified-nodes)
                               unified-nodes (conj unified-nodes new-node)
                               node-remap (assoc node-remap [src-idx ni] new-idx)
                               unified-nodes (if parent-new
                                               (update unified-nodes parent-new
                                                       #(update % :children (fnil conj []) new-idx))
                                               unified-nodes)]
                           [unified-nodes node-remap])))
                     [unified-nodes node-remap]
                     (:node-indices (:part src)))
                    ;; Re-link children for appended nodes.
                    unified-nodes
                    (reduce
                     (fn [unified-nodes ni]
                       (if-let [new-ni (get node-remap [src-idx ni])]
                         (if (>= new-ni (count (:nodes base-gltf)))
                           (let [old-children (:children (get (:nodes src-gltf) ni))
                                 new-children (vec (keep #(get node-remap [src-idx %]) old-children))]
                             (assoc-in unified-nodes [new-ni :children] new-children))
                           unified-nodes)
                         unified-nodes))
                     unified-nodes
                     (:node-indices (:part src)))]
                [unified-nodes node-remap])))

          [unified-nodes node-remap]
          (reduce (fn [acc [src-idx src]] (unify-source acc src-idx src))
                   [(vec (:nodes base-gltf)) node-remap0]
                   (map-indexed vector sources))

          ;; ── Phase 2: buffer merging ── (keyed by canonical doc index `ci`,
          ;; and SKIPPED entirely once a document's buffer/accessor data has
          ;; already been merged -- see the dedup comment above. Without the
          ;; `seen` guard, re-running this per-part would still just re-copy
          ;; the same document's :bin again under a different byte offset.)
          buffer-merge
          (reduce
           (fn [{:keys [bin buffer-view-remap accessor-remap buffer-views accessors seen] :as acc} [src-idx src]]
             (let [ci (canon src-idx)]
               (if (contains? seen ci)
                 acc
                 (let [src-doc (:doc src)
                       src-gltf (:gltf src-doc)
                       base-offset (count bin)
                       bin (into bin (:bin src-doc))
                       pad (mod (- 4 (mod (count bin) 4)) 4)
                       bin (into bin (repeat pad 0))
                       [buffer-view-remap buffer-views]
                       (reduce
                        (fn [[bvr bvs] [bv-idx bv]]
                          (let [new-bv-idx (count bvs)]
                            [(assoc bvr [ci bv-idx] new-bv-idx)
                             (conj bvs {:buffer 0
                                        :byteOffset (+ (or (:byteOffset bv) 0) base-offset)
                                        :byteLength (:byteLength bv)
                                        :byteStride (:byteStride bv)
                                        :target (:target bv)})]))
                        [buffer-view-remap buffer-views]
                        (map-indexed vector (:bufferViews src-gltf)))
                       [accessor-remap accessors]
                       (reduce
                        (fn [[ar accs] [acc-idx acc-val]]
                          (let [new-acc-idx (count accs)
                                new-bv (when-let [bv (:bufferView acc-val)] (get buffer-view-remap [ci bv]))]
                            [(assoc ar [ci acc-idx] new-acc-idx)
                             (conj accs {:bufferView new-bv
                                         :componentType (:componentType acc-val)
                                         :count (:count acc-val)
                                         :type (:type acc-val)
                                         :byteOffset (or (:byteOffset acc-val) 0)
                                         :min (:min acc-val)
                                         :max (:max acc-val)
                                         :normalized (boolean (:normalized acc-val))})]))
                        [accessor-remap accessors]
                        (map-indexed vector (:accessors src-gltf)))]
                   {:bin bin :buffer-view-remap buffer-view-remap :accessor-remap accessor-remap
                    :buffer-views buffer-views :accessors accessors :seen (conj seen ci)}))))
           {:bin [] :buffer-view-remap {} :accessor-remap {} :buffer-views [] :accessors [] :seen #{}}
           (map-indexed vector sources))
          {:keys [bin buffer-view-remap accessor-remap buffer-views accessors]} buffer-merge

          ;; ── Phase 3/4: mesh/material/texture/image merging ── (`ci` =
          ;; canonical doc index -- images/textures/materials are keyed and
          ;; deduped by `ci`, since they belong to a whole SOURCE DOCUMENT and
          ;; different parts sharing one document must resolve to the SAME
          ;; merged copy; mesh-remap/node-remap stay keyed by the real
          ;; `src-idx` below, since mesh/node reachability is genuinely
          ;; per-PART, not per-document.)
          merge-result
          (reduce
           (fn [{:keys [images image-remap samplers textures texture-remap
                        materials material-remap meshes mesh-remap unified-nodes] :as acc}
                [src-idx src]]
             (let [src-doc (:doc src)
                   src-gltf (:gltf src-doc)
                   ci (canon src-idx)
                   ;; Images
                   [images image-remap]
                   (reduce
                    (fn [[imgs ir] [img-idx img]]
                      (if (contains? ir [ci img-idx])
                        [imgs ir]
                        (let [new-idx (count imgs)
                              new-img (cond-> img
                                        (:bufferView img)
                                        (assoc :bufferView (get buffer-view-remap [ci (:bufferView img)])))]
                          [(conj imgs new-img) (assoc ir [ci img-idx] new-idx)])))
                    [images image-remap]
                    (map-indexed vector (:images src-gltf)))
                   sampler-base (count samplers)
                   samplers (into samplers (:samplers src-gltf))
                   ;; Textures
                   [textures texture-remap]
                   (reduce
                    (fn [[txs tr] [tex-idx tex]]
                      (if (contains? tr [ci tex-idx])
                        [txs tr]
                        (let [new-idx (count txs)]
                          [(conj txs {:sampler (when (:sampler tex) (+ (:sampler tex) sampler-base))
                                      :source (when (:source tex) (get image-remap [ci (:source tex)]))})
                           (assoc tr [ci tex-idx] new-idx)])))
                    [textures texture-remap]
                    (map-indexed vector (:textures src-gltf)))
                   ;; Materials
                   [materials material-remap]
                   (reduce
                    (fn [[mats mr] mat-idx]
                      (if (contains? mr [ci mat-idx])
                        [mats mr]
                        (if-let [mat (get (:materials src-gltf) mat-idx)]
                          (let [new-idx (count mats)
                                remap-tex (fn [ti] (get texture-remap [ci ti] ti))
                                new-mat (cond-> mat
                                          (get-in mat [:pbrMetallicRoughness :baseColorTexture :index])
                                          (update-in [:pbrMetallicRoughness :baseColorTexture :index] remap-tex)
                                          (get-in mat [:pbrMetallicRoughness :metallicRoughnessTexture :index])
                                          (update-in [:pbrMetallicRoughness :metallicRoughnessTexture :index] remap-tex))]
                            [(conj mats new-mat) (assoc mr [ci mat-idx] new-idx)])
                          [mats mr])))
                    [materials material-remap]
                    (:material-indices (:part src)))
                   ;; Meshes (kept keyed by the real `src-idx` for mesh-remap
                   ;; itself -- see the comment above this reduce -- but its
                   ;; internal accessor/material LOOKUPS must use `ci`, since
                   ;; those two tables are now ci-keyed.)
                   [meshes mesh-remap unified-nodes]
                   (reduce
                    (fn [[ms mr nodes] mi]
                      (if-let [mesh (get (:meshes src-gltf) mi)]
                        (let [new-mi (count ms)
                              remap-attr-map
                              (fn [attrs]
                                (into {} (map (fn [[k v]]
                                                (if (number? v)
                                                  [k (get accessor-remap [ci v] v)]
                                                  [k v])))
                                      attrs))
                              new-mesh (update mesh :primitives
                                               (fn [prims]
                                                 (mapv (fn [prim]
                                                         (cond-> prim
                                                           true (update :attributes remap-attr-map)
                                                           (:indices prim) (update :indices #(get accessor-remap [ci %] %))
                                                           (:material prim) (update :material #(get material-remap [ci %] %))
                                                           (seq (:targets prim)) (update :targets #(mapv remap-attr-map %))))
                                                       prims)))
                              ms (conj ms new-mesh)
                              mr (assoc mr [src-idx mi] new-mi)
                              ;; Attach mesh to correct node.
                              owning-node (some (fn [[ni n]] (when (= (:mesh n) mi) ni))
                                                 (map-indexed vector (:nodes src-gltf)))
                              nodes (if (and owning-node (some #{owning-node} (:node-indices (:part src))))
                                      (if-let [new-ni (get node-remap [src-idx owning-node])]
                                        (if (< new-ni (count nodes))
                                          (assoc-in nodes [new-ni :mesh] new-mi)
                                          nodes)
                                        nodes)
                                      nodes)]
                          [ms mr nodes])
                        [ms mr nodes]))
                    [meshes mesh-remap unified-nodes]
                    (:mesh-indices (:part src)))]
               {:images images :image-remap image-remap :samplers samplers
                :textures textures :texture-remap texture-remap
                :materials materials :material-remap material-remap
                :meshes meshes :mesh-remap mesh-remap :unified-nodes unified-nodes}))
           {:images [] :image-remap {} :samplers [] :textures [] :texture-remap {}
            :materials [] :material-remap {} :meshes [] :mesh-remap {} :unified-nodes unified-nodes}
           (map-indexed vector sources))
          {:keys [images samplers textures texture-remap materials material-remap
                  meshes mesh-remap unified-nodes]} merge-result

          ;; ── Phase 5: skin rebuild ──
          ;; joint-set grows beyond the base skin's own joint list whenever a donor
          ;; part contributes a joint the base doesn't have (e.g. hair-specific bones).
          ;; ibm-plan tracks, PARALLEL to joint-set, where each joint's inverse bind
          ;; matrix should be read from ({:src-idx .. :pos ..}, `pos` = that joint's
          ;; position within ITS OWN source skin's `:joints` list, since a skin's
          ;; inverseBindMatrices accessor is ordered by joint POSITION, not node id).
          base-skin (first (:skins base-gltf))
          [joint-set ibm-plan]
          (if-not base-skin
            [nil nil]
            (reduce
             (fn [[joints plan] [src-idx src]]
               (if (= src-idx skeleton-base)
                 [joints plan]
                 (if-let [src-skin (first (:skins (:gltf (:doc src))))]
                   (reduce
                    (fn [[joints plan] [old-pos old-joint]]
                      (if-let [new-joint (get node-remap [src-idx old-joint])]
                        (if (some #{new-joint} joints)
                          [joints plan]
                          [(conj joints new-joint) (conj plan {:src-idx src-idx :pos old-pos})])
                        [joints plan]))
                    [joints plan]
                    (map-indexed vector (:joints src-skin)))
                   [joints plan])))
             [(vec (:joints base-skin))
              (vec (map-indexed (fn [i _] {:src-idx skeleton-base :pos i}) (:joints base-skin)))]
             (map-indexed vector sources)))

          ;; A real bug, found via net-babiniku's M6 slice-2 part-swap (composing
          ;; Seed-san as base + a donor's hair): reusing the base's own (shorter)
          ;; inverseBindMatrices accessor here, unchanged, left babiniku.vrm-bridge's
          ;; joint-palette builder reading past the end of that accessor's data once it
          ;; walked past the base's original joint count -- "No item 23 in vector of
          ;; length 23". A skin's joints and inverseBindMatrices must be the SAME
          ;; length; when donor joints grow the list, a fresh accessor spanning the
          ;; full joint-set (base IBMs first, then each donor's, in append order) is
          ;; built and appended to the merged buffer -- not a shortcut around the real
          ;; data, an accessor with the SAME real IBM values just concatenated.
          skin-cache (atom {})
          base-ibm-idx (when base-skin (:inverseBindMatrices base-skin))
          base-ibm-floats (when base-ibm-idx (conv/read-accessor-f32 base-doc base-ibm-idx))
          ibm-floats-for
          (fn [{:keys [src-idx pos]}]
            (if (= src-idx skeleton-base)
              (subvec (or base-ibm-floats []) (* pos 16) (+ (* pos 16) 16))
              (let [src-doc (:doc (nth sources src-idx))
                    src-skin (or (get @skin-cache src-idx)
                                 (let [s (first (:skins (:gltf src-doc)))]
                                   (swap! skin-cache assoc src-idx s)
                                   s))
                    ibm-idx (:inverseBindMatrices src-skin)]
                (if ibm-idx
                  (subvec (conv/read-accessor-f32 src-doc ibm-idx) (* pos 16) (+ (* pos 16) 16))
                  ;; No IBM data at all for this donor skin: identity, same fallback
                  ;; `skin-joint-palette` already applies when a skin lacks one.
                  [1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0]))))
          grew? (and base-skin (> (count joint-set) (count (:joints base-skin))))
          [bin buffer-views accessors unified-skin]
          (cond
            (not base-skin) [bin buffer-views accessors nil]
            (not grew?)
            [bin buffer-views accessors
             {:name (:name base-skin)
              :joints joint-set
              :inverseBindMatrices (when base-ibm-idx (get accessor-remap [skeleton-base base-ibm-idx]))
              :skeleton (when-let [s (:skeleton base-skin)] (get node-remap [skeleton-base s]))}]
            :else
            (let [all-floats (vec (mapcat ibm-floats-for ibm-plan))
                  new-bytes (vec (mapcat write-f32-le all-floats))
                  base-offset (count bin)
                  bin (into bin new-bytes)
                  new-bv-idx (count buffer-views)
                  buffer-views (conj buffer-views {:buffer 0 :byteOffset base-offset
                                                   :byteLength (count new-bytes)})
                  new-acc-idx (count accessors)
                  accessors (conj accessors {:bufferView new-bv-idx
                                             :componentType gt/component-type-float
                                             :count (count joint-set)
                                             :type "MAT4"
                                             :byteOffset 0})]
              [bin buffer-views accessors
               {:name (:name base-skin)
                :joints joint-set
                :inverseBindMatrices new-acc-idx
                :skeleton (when-let [s (:skeleton base-skin)] (get node-remap [skeleton-base s]))}]))
          unified-nodes
          (if unified-skin
            (mapv (fn [n] (if (:mesh n) (assoc n :skin 0) n)) unified-nodes)
            unified-nodes)

          ;; ── Phase 6: spring bone merging ──
          spring-merge
          (reduce
           (fn [{:keys [colliders collider-remap collider-groups collider-group-remap spring-bones] :as acc}
                [src-idx src]]
             (let [src-doc (:doc src)
                   [colliders collider-remap]
                   (reduce
                    (fn [[cs cr] ci]
                      (if-let [collider (get (:spring-bone-colliders src-doc) ci)]
                        (let [new-ci (count cs)]
                          [(conj cs (assoc collider :node (get node-remap [src-idx (:node collider)] (:node collider))))
                           (assoc cr [src-idx ci] new-ci)])
                        [cs cr]))
                    [colliders collider-remap]
                    (:collider-indices (:part src)))
                   [collider-groups collider-group-remap]
                   (reduce
                    (fn [[cgs cgr] [gi group]]
                      (if (some (set (:collider-indices (:part src))) (:colliders group))
                        (let [new-gi (count cgs)]
                          [(conj cgs {:name (:name group)
                                      :colliders (vec (keep #(get collider-remap [src-idx %]) (:colliders group)))})
                           (assoc cgr [src-idx gi] new-gi)])
                        [cgs cgr]))
                    [collider-groups collider-group-remap]
                    (map-indexed vector (:spring-bone-collider-groups src-doc)))
                   spring-bones
                   (reduce
                    (fn [sbs sbi]
                      (if-let [chain (get (:spring-bones src-doc) sbi)]
                        (conj sbs
                              {:name (:name chain)
                               :joints (mapv (fn [j] (assoc j :node (get node-remap [src-idx (:node j)] (:node j))))
                                             (:joints chain))
                               :collider-groups (vec (keep #(get collider-group-remap [src-idx %]) (:collider-groups chain)))
                               :center (when-let [c (:center chain)] (get node-remap [src-idx c]))})
                        sbs))
                    spring-bones
                    (:spring-bone-indices (:part src)))]
               {:colliders colliders :collider-remap collider-remap
                :collider-groups collider-groups :collider-group-remap collider-group-remap
                :spring-bones spring-bones}))
           {:colliders [] :collider-remap {} :collider-groups [] :collider-group-remap {} :spring-bones []}
           (map-indexed vector sources))
          {:keys [colliders collider-groups spring-bones]} spring-merge

          ;; ── Phase 7: expression merging ── (mesh-index lookups stay
          ;; `src-idx`-keyed via `mesh-remap`, matching Phase 3/4's per-part
          ;; mesh-remap; material-index lookups use `ci`, matching Phase
          ;; 3/4's ci-keyed `material-remap`.)
          expressions
          (reduce
           (fn [unified [src-idx src]]
             (let [src-doc (:doc src)
                   ci (canon src-idx)]
               (reduce
                (fn [unified ei]
                  (if-let [expr (get (:expressions src-doc) ei)]
                    (let [existing-idx (when (:preset expr)
                                          (some (fn [[i e]] (when (= (:preset e) (:preset expr)) i))
                                                (map-indexed vector unified)))]
                      (if existing-idx
                        (-> unified
                            (update-in [existing-idx :morph-target-binds]
                                       into (map (fn [b] (assoc b :mesh-index (get mesh-remap [src-idx (:mesh-index b)] (:mesh-index b))))
                                                 (:morph-target-binds expr)))
                            (update-in [existing-idx :material-color-binds]
                                       into (map (fn [b] (assoc b :material-index (get material-remap [ci (:material-index b)] (:material-index b))))
                                                 (:material-color-binds expr))))
                        (conj unified
                              (-> expr
                                  (update :morph-target-binds
                                          (fn [bs] (mapv #(assoc % :mesh-index (get mesh-remap [src-idx (:mesh-index %)] (:mesh-index %))) bs)))
                                  (update :material-color-binds
                                          (fn [bs] (mapv #(assoc % :material-index (get material-remap [ci (:material-index %)] (:material-index %))) bs)))
                                  (update :texture-transform-binds
                                          (fn [bs] (mapv #(assoc % :material-index (get material-remap [ci (:material-index %)] (:material-index %))) bs)))))))
                    unified))
                unified
                (:expression-indices (:part src)))))
           []
           (map-indexed vector sources))

          ;; ── Phase 8: MToon material merging ── (ci-keyed, matching Phase
          ;; 3/4's ci-keyed `material-remap`/`texture-remap`.)
          mtoon
          (reduce
           (fn [unified [src-idx src]]
             (let [src-doc (:doc src)
                   ci (canon src-idx)]
               (into unified
                     (comp
                      (filter #(some #{(:material-index %)} (:material-indices (:part src))))
                      (map (fn [m]
                             (-> m
                                 (assoc :material-index (get material-remap [ci (:material-index m)] (:material-index m)))
                                 (update :shade-multiply-texture #(when % (get texture-remap [ci %])))
                                 (update :rim-multiply-texture #(when % (get texture-remap [ci %])))
                                 (update :matcap-texture #(when % (get texture-remap [ci %])))))))
                     (:mtoon-materials src-doc))))
           []
           (map-indexed vector sources))

          ;; ── Build unified scene ──
          scene-nodes (or (some-> (:scenes base-gltf) first :nodes) [0])
          unified-gltf
          (gt/gltf-document
           {:asset (gt/asset {:generator "kami-vrm"})
            :scene 0
            :scenes [{:nodes scene-nodes}]
            :nodes unified-nodes
            :meshes meshes
            :accessors accessors
            :bufferViews buffer-views
            :buffers [{:byteLength (count bin)}]
            :materials materials
            :textures textures
            :images images
            :samplers samplers
            :skins (if unified-skin [unified-skin] [])
            :animations []
            :extensionsUsed ["VRMC_vrm" "VRMC_springBone" "VRMC_materials_mtoon"]
            :extensionsRequired []
            :extensions nil})]
      (vt/vrm-document
       {:gltf unified-gltf
        :bin bin
        :version :v1-0
        :meta (:meta base-doc)
        :humanoid (vt/vrm-humanoid
                   (mapv (fn [hb] (vt/vrm-human-bone (:bone hb) (get node-remap [skeleton-base (:node hb)] (:node hb))))
                         (:human-bones (:humanoid base-doc))))
        :expressions expressions
        :spring-bones spring-bones
        :spring-bone-colliders colliders
        :spring-bone-collider-groups collider-groups
        :mtoon-materials mtoon
        :look-at (:look-at base-doc)
        :first-person (:first-person base-doc)
        :node-constraints []}))))
