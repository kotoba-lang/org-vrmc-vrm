(ns vrm-test
  "Restoration-fidelity tests — one per original `kami-vrm/src/lib.rs`
  `#[cfg(test)] mod tests` (kotoba-lang/kami-engine, deleted PR #82), plus
  the namespace-loads smoke test."
  (:require [clojure.test :refer [deftest is testing]]
            [vrm]
            [vrm.glb :as glb]
            [vrm.json :as json]
            [vrm.humanoid :as humanoid]
            [vrm.convert :as conv]))

(deftest namespace-loads
  (testing "the restored CLJC namespace loads"
    (is (some? (the-ns 'vrm)))))

;; mirrors `make_test_vrm` — a minimal VRM 1.0 GLB.
(defn- make-test-vrm []
  (let [json-map
        {:asset {:version "2.0" :generator "kami-vrm-test"}
         :extensionsUsed ["VRMC_vrm"]
         :scene 0
         :scenes [{:nodes [0]}]
         :nodes [{:name "Root" :children [1 2]}
                 {:name "Hips" :mesh 0 :skin 0 :translation [0 0.8 0]}
                 {:name "Head" :mesh 1 :translation [0 0.4 0]}]
         :meshes [{:name "Body" :primitives [{:attributes {:POSITION 0} :indices 1 :material 0}]}
                  {:name "Hair" :primitives [{:attributes {:POSITION 2} :indices 3 :material 1}]}]
         :materials [{:name "skin_material" :pbrMetallicRoughness {:baseColorFactor [0.9 0.7 0.6 1.0]}}
                     {:name "hair_material" :pbrMetallicRoughness {:baseColorFactor [0.2 0.1 0.05 1.0]}}]
         :accessors [{:bufferView 0 :componentType 5126 :count 3 :type "VEC3" :min [-0.5 -0.5 -0.5] :max [0.5 0.5 0.5]}
                     {:bufferView 1 :componentType 5125 :count 3 :type "SCALAR"}
                     {:bufferView 2 :componentType 5126 :count 3 :type "VEC3" :min [-0.3 0.0 -0.3] :max [0.3 0.5 0.3]}
                     {:bufferView 3 :componentType 5125 :count 3 :type "SCALAR"}
                     {:bufferView 4 :componentType 5126 :count 3 :type "MAT4"}]
         :bufferViews [{:buffer 0 :byteOffset 0 :byteLength 36}
                       {:buffer 0 :byteOffset 36 :byteLength 12}
                       {:buffer 0 :byteOffset 48 :byteLength 36}
                       {:buffer 0 :byteOffset 84 :byteLength 12}
                       {:buffer 0 :byteOffset 96 :byteLength 192}]
         :buffers [{:byteLength 288}]
         :skins [{:joints [0 1 2] :inverseBindMatrices 4}]
         :extensions {:VRMC_vrm {:specVersion "1.0"
                                  :meta {:name "TestAvatar" :authors ["test"]
                                         :licenseUrl "https://vrm.dev/licenses/1.0/"
                                         :avatarPermission "everyone"}
                                  :humanoid {:humanBones {:hips {:node 1} :head {:node 2}}}
                                  ;; real VRM 1.0 morphTargetBind schema: {node index weight}
                                  ;; (vrm-specification/VRMC_vrm-1.0/expressions.md) -- node 1
                                  ;; ("Hips") has :mesh 0, so this should resolve to mesh-index 0.
                                  :expressions {:preset {:happy {:isBinary false
                                                                  :morphTargetBinds [{:node 1 :index 0 :weight 1.0}]}}}}}}
        f32->bytes (fn [f] (glb/u32->le-bytes
                            #?(:clj (Float/floatToIntBits (float f))
                               :cljs (let [buf (js/ArrayBuffer. 4) view (js/DataView. buf)]
                                       (.setFloat32 view 0 f true)
                                       (bit-or (.getUint8 view 0)
                                               (bit-shift-left (.getUint8 view 1) 8)
                                               (bit-shift-left (.getUint8 view 2) 16)
                                               (bit-shift-left (.getUint8 view 3) 24))))))
        body-pos (mapcat f32->bytes [-0.5 0.0 0.0 0.5 0.0 0.0 0.0 1.0 0.0])
        body-idx (mapcat glb/u32->le-bytes [0 1 2])
        hair-pos (mapcat f32->bytes [-0.3 0.0 0.0 0.3 0.0 0.0 0.0 0.5 0.0])
        hair-idx (mapcat glb/u32->le-bytes [0 1 2])
        identity-mat4 [1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0]
        ibm (mapcat (fn [_] (mapcat f32->bytes identity-mat4)) (range 3))
        bin (vec (concat body-pos body-idx hair-pos hair-idx ibm))
        json-bytes (glb/string->byte-seq (json/->json json-map))]
    (glb/write-glb json-bytes bin)))

;; mirrors `parse_test_vrm`
(deftest parse-test-vrm
  (let [doc (vrm/parse-vrm (make-test-vrm))]
    (is (= :v1-0 (:version doc)))
    (is (= "TestAvatar" (:name (:meta doc))))
    (is (= 2 (count (:human-bones (:humanoid doc)))))
    (is (= 2 (count (:meshes (:gltf doc)))))))

;; mirrors `decompose_test_vrm`
(deftest decompose-test-vrm
  (let [doc (vrm/parse-vrm (make-test-vrm))
        parts (vrm/decompose doc)]
    (is (>= (count parts) 2))
    (is (some #(= :body (:category %)) parts))
    (is (some #(= :hair (:category %)) parts))))

;; mirrors `compose_and_export_roundtrip`
(deftest compose-and-export-roundtrip
  (let [doc (vrm/parse-vrm (make-test-vrm))
        parts (vrm/decompose doc)
        sources (mapv (fn [p] {:part p :doc doc}) parts)
        composed (vrm/compose-parts sources {:skeleton-base 0})
        output (vrm/export-glb composed)
        chunks (glb/parse-glb output)]
    (is (seq (:json chunks)))
    (let [reparsed (vrm/parse-vrm output)]
      (is (= "TestAvatar" (:name (:meta reparsed))))
      (is (>= (count (:human-bones (:humanoid reparsed))) 2)))
    ;; Real bug fix regression guard (/loop maturity pass): every `source`
    ;; here shares the SAME `doc` (both the "Body" and "Hair" parts come from
    ;; one parsed test VRM) -- exactly the shape a real character-creator mix
    ;; produces when multiple picked categories come from one uploaded file.
    ;; Before the fix, `compose`'s buffer-merge concatenated a shared source
    ;; document's full `:bin` once PER PART referencing it (2x here, since 2
    ;; parts share 1 doc) instead of once per UNIQUE document -- confirmed via
    ;; a real two-file measurement that exported 54MB from ~21MB of actual
    ;; source data (exactly 4x one file's :bin + 1x the other's, matching 4
    ;; parts sharing one doc + 1 part from the second). The composed `:bin`
    ;; must equal the ORIGINAL doc's `:bin` length exactly here, not some
    ;; multiple of it.
    (is (= (count (:bin doc)) (count (:bin composed)))
        "composed :bin must not duplicate a source document shared by multiple parts")))

;; A second synthetic VRM whose skin has a 4th joint ("HairTip", child of Head, no
;; humanoid-bone mapping) that `make-test-vrm`'s skeleton doesn't have -- for the
;; skin-inverseBindMatrices-growth test below. Its 4th (last) inverse bind matrix is a
;; distinctive non-identity value (translation [9 8 7]) so the growth test can assert
;; the EXACT donor data survived compose+export+reparse at the right position, not just
;; that nothing threw.
(defn- make-test-vrm-2 []
  (let [json-map
        {:asset {:version "2.0" :generator "kami-vrm-test"}
         :extensionsUsed ["VRMC_vrm"]
         :scene 0
         :scenes [{:nodes [0]}]
         :nodes [{:name "Root" :children [1 2]}
                 {:name "Hips" :mesh 0 :skin 0 :translation [0 0.8 0]}
                 {:name "Head" :mesh 1 :skin 0 :translation [0 0.4 0] :children [3]}
                 {:name "HairTip" :translation [0 0.1 0]}]
         :meshes [{:name "Body" :primitives [{:attributes {:POSITION 0} :indices 1 :material 0}]}
                  {:name "Hair" :primitives [{:attributes {:POSITION 2} :indices 3 :material 1}]}]
         :materials [{:name "skin_material" :pbrMetallicRoughness {:baseColorFactor [0.9 0.7 0.6 1.0]}}
                     {:name "hair_material" :pbrMetallicRoughness {:baseColorFactor [0.2 0.1 0.05 1.0]}}]
         :accessors [{:bufferView 0 :componentType 5126 :count 3 :type "VEC3" :min [-0.5 -0.5 -0.5] :max [0.5 0.5 0.5]}
                     {:bufferView 1 :componentType 5125 :count 3 :type "SCALAR"}
                     {:bufferView 2 :componentType 5126 :count 3 :type "VEC3" :min [-0.3 0.0 -0.3] :max [0.3 0.5 0.3]}
                     {:bufferView 3 :componentType 5125 :count 3 :type "SCALAR"}
                     {:bufferView 4 :componentType 5126 :count 4 :type "MAT4"}]
         :bufferViews [{:buffer 0 :byteOffset 0 :byteLength 36}
                       {:buffer 0 :byteOffset 36 :byteLength 12}
                       {:buffer 0 :byteOffset 48 :byteLength 36}
                       {:buffer 0 :byteOffset 84 :byteLength 12}
                       {:buffer 0 :byteOffset 96 :byteLength 256}]
         :buffers [{:byteLength 352}]
         :skins [{:joints [0 1 2 3] :inverseBindMatrices 4}]
         :extensions {:VRMC_vrm {:specVersion "1.0"
                                  :meta {:name "DonorAvatar" :authors ["test"]
                                         :licenseUrl "https://vrm.dev/licenses/1.0/"
                                         :avatarPermission "everyone"}
                                  :humanoid {:humanBones {:hips {:node 1} :head {:node 2}}}}}}
        f32->bytes (fn [f] (glb/u32->le-bytes
                            #?(:clj (Float/floatToIntBits (float f))
                               :cljs (let [buf (js/ArrayBuffer. 4) view (js/DataView. buf)]
                                       (.setFloat32 view 0 f true)
                                       (bit-or (.getUint8 view 0)
                                               (bit-shift-left (.getUint8 view 1) 8)
                                               (bit-shift-left (.getUint8 view 2) 16)
                                               (bit-shift-left (.getUint8 view 3) 24))))))
        body-pos (mapcat f32->bytes [-0.5 0.0 0.0 0.5 0.0 0.0 0.0 1.0 0.0])
        body-idx (mapcat glb/u32->le-bytes [0 1 2])
        hair-pos (mapcat f32->bytes [-0.3 0.0 0.0 0.3 0.0 0.0 0.0 0.5 0.0])
        hair-idx (mapcat glb/u32->le-bytes [0 1 2])
        identity-mat4 [1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0]
        ;; joints 0/1/2 (Root/Hips/Head): identity. Joint 3 (HairTip, the only one
        ;; compose's fix ever reads from THIS donor): translation [9 8 7] -- distinctive
        ;; and easy to assert on exactly.
        hair-tip-mat4 [1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0 9.0 8.0 7.0 1.0]
        ibm (mapcat (fn [m] (mapcat f32->bytes m)) [identity-mat4 identity-mat4 identity-mat4 hair-tip-mat4])
        bin (vec (concat body-pos body-idx hair-pos hair-idx ibm))
        json-bytes (glb/string->byte-seq (json/->json json-map))]
    (glb/write-glb json-bytes bin)))

;; Real bug regression (net-babiniku ADR-2607071600 M6 slice-2, 2026-07-07): compose's
;; skin rebuild used to reuse the BASE skin's own (shorter) inverseBindMatrices accessor
;; unchanged even when donor parts grew the joint list past it -- a real avatar's
;; joint-palette builder then read past the end of that accessor's data ("No item 23 in
;; vector of length 23", found composing VRM Consortium's Seed-san as a base + a
;; donor's hair). This reproduces the SAME shape (base skin's joints grow when a donor
;; part contributes a joint the base doesn't have) on a minimal synthetic fixture and
;; asserts the fix: the composed skin's joints and inverseBindMatrices must be the SAME
;; length, and the appended joint's REAL inverse bind matrix data (not a placeholder)
;; must survive a full compose -> export -> reparse round-trip at the right position.
(deftest compose-grows-inverse-bind-matrices-with-donor-joints
  (let [base-doc (vrm/parse-vrm (make-test-vrm))
        donor-doc (vrm/parse-vrm (make-test-vrm-2))
        body-part (some #(when (= :body (:category %)) %) (vrm/decompose base-doc))
        hair-part (some #(when (= :hair (:category %)) %) (vrm/decompose donor-doc))
        sources [{:part body-part :doc base-doc} {:part hair-part :doc donor-doc}]
        composed (vrm/compose-parts sources {:skeleton-base 0})
        skin (first (get-in composed [:gltf :skins]))]
    (testing "the donor's extra joint (HairTip) was appended to the base's joint list"
      (is (= 4 (count (:joints skin)))
          "base had 3 joints (Root/Hips/Head); the donor's non-humanoid HairTip joint should add exactly 1"))
    (testing "joints and inverseBindMatrices stay the same length -- the actual bug"
      (let [ibm-idx (:inverseBindMatrices skin)
            ibm-accessor (get-in composed [:gltf :accessors ibm-idx])]
        (is (= (count (:joints skin)) (:count ibm-accessor))
            "an accessor shorter than :joints is exactly what made real-avatar loading throw \"No item N in vector of length N\"")))
    (testing "the appended joint's REAL inverse bind matrix survives export + reparse, at the right position"
      (let [output (vrm/export-glb composed)
            reparsed (vrm/parse-vrm output)
            skin2 (first (get-in reparsed [:gltf :skins]))
            ibm-idx2 (:inverseBindMatrices skin2)
            floats (conv/read-accessor-f32 reparsed ibm-idx2)]
        (is (= 64 (count floats)) "4 joints * 16 floats each")
        ;; the last joint in the (grown) joint list is the appended HairTip -- its
        ;; matrix's translation column (last 4 floats of its 16) must be [9 8 7 1],
        ;; the exact value from donor-doc's own inverseBindMatrices, not identity/
        ;; zeroed/any other placeholder.
        (is (= [9.0 8.0 7.0 1.0] (subvec floats 60 64))
            "the donor's real IBM data for the appended joint, not a placeholder")))))

;; Document-dedup fix, isolated: two parts from the SAME doc (already covered
;; above) vs. parts genuinely spanning two DIFFERENT docs -- this asserts the
;; dedup-by-`identical?` fix doesn't over-merge distinct documents that just
;; happen to be structurally similar (a second, separately-parsed instance of
;; the exact same bytes is a DIFFERENT object, must NOT be deduped away).
(deftest compose-distinct-docs-not-deduped
  (let [bytes (make-test-vrm)
        doc-a (vrm/parse-vrm bytes)
        doc-b (vrm/parse-vrm bytes) ;; a second, separate parse of the same bytes -- NOT `identical?` to doc-a
        parts-a (vrm/decompose doc-a)
        parts-b (vrm/decompose doc-b)
        body-a (some #(when (= :body (:category %)) %) parts-a)
        hair-b (some #(when (= :hair (:category %)) %) parts-b)
        sources [{:part body-a :doc doc-a} {:part hair-b :doc doc-b}]
        composed (vrm/compose-parts sources {:skeleton-base 0})]
    ;; two genuinely distinct (non-identical!) doc instances -> both must be
    ;; merged, so composed :bin is the sum of both, not deduped down to one.
    (is (= (+ (count (:bin doc-a)) (count (:bin doc-b))) (count (:bin composed)))
        "two distinct doc instances (even with identical bytes) must both be merged, not deduped")
    (is (= 2 (count (get-in composed [:gltf :meshes]))))))

;; mirrors `skeleton_extraction`
(deftest skeleton-extraction
  (let [doc (vrm/parse-vrm (make-test-vrm))
        skeleton (humanoid/to-kami-skeleton doc)]
    (is (= 3 (count (:bones skeleton))))))

;; Real bug fix (/loop maturity pass, ADR-2607031200): VRM 1.0's
;; morphTargetBinds.node is a NODE index that must resolve to that node's
;; :mesh -- parse-single-expression used to read a nonexistent `:mesh` key
;; directly (VRM 0.x's *different* blendshape schema genuinely does use
;; `:mesh`, which is why `vrm.compat`'s parallel code was correct and
;; untouched), so morph-target-binds silently parsed empty for every real
;; VRM 1.0 file -- confirmed against a real production VRM during this
;; session's investigation. No existing fixture in this suite ever set
;; :expressions at all, so nothing caught it until now.
(deftest expression-morph-target-bind-resolves-node-to-mesh
  (testing "a VRM 1.0 morphTargetBind's :node resolves to that node's :mesh"
    (let [doc (vrm/parse-vrm (make-test-vrm))
          happy (first (filter #(= :happy (:preset %)) (:expressions doc)))]
      (is (some? happy))
      (is (= 1 (count (:morph-target-binds happy))))
      (let [bind (first (:morph-target-binds happy))]
        ;; node 1 ("Hips") has :mesh 0 in the fixture -- this must resolve
        ;; to mesh-index 0, not silently drop the bind.
        (is (= 0 (:mesh-index bind)))
        (is (= 0 (:morph-index bind)))
        (is (= 1.0 (:weight bind)))))))
