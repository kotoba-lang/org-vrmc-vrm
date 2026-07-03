(ns vrm.convert
  "Conversions between kami-vrm data and vertex/index buffers ready for GPU
  upload (the kami-render analogue). Restored from `kami-vrm/src/convert.rs`
  (kotoba-lang/kami-engine, deleted PR #82) as part of the clj-wgsl migration
  (ADR-2607010930, com-junkawasaki/root)."
  (:require [vrm.gltf-types :as gt]))

(defn- read-f32-le [bin o]
  ;; `bit-or`/`bit-shift-left` operate on Clojure `long`s, so a set top bit
  ;; (any negative float, or a huge-magnitude one) produces a value > Integer/
  ;; MAX_VALUE; `Float/intBitsToFloat` requires a genuine 32-bit `int`, and
  ;; Clojure's auto-narrowing throws ArithmeticException instead of wrapping.
  ;; `unchecked-int` does the two's-complement truncation Rust's `f32::from_bits`
  ;; gets for free from `u32` — this is a real bug fix (any negative vertex
  ;; coordinate crashes `read-accessor-f32` on JVM without it), not a port
  ;; deviation: the CLJS branch below was already correct via DataView.
  #?(:clj (Float/intBitsToFloat
           (unchecked-int
            (bit-or (bit-and (nth bin o) 0xFF)
                    (bit-shift-left (bit-and (nth bin (+ o 1)) 0xFF) 8)
                    (bit-shift-left (bit-and (nth bin (+ o 2)) 0xFF) 16)
                    (bit-shift-left (bit-and (nth bin (+ o 3)) 0xFF) 24))))
     :cljs (let [buf (js/ArrayBuffer. 4) view (js/DataView. buf)]
             (.setUint8 view 0 (nth bin o)) (.setUint8 view 1 (nth bin (+ o 1)))
             (.setUint8 view 2 (nth bin (+ o 2))) (.setUint8 view 3 (nth bin (+ o 3)))
             (.getFloat32 view 0 true))))

(defn- read-u16-le [bin o]
  (bit-or (bit-and (nth bin o) 0xFF) (bit-shift-left (bit-and (nth bin (+ o 1)) 0xFF) 8)))

(defn- read-u32-le [bin o]
  (bit-or (bit-and (nth bin o) 0xFF)
          (bit-shift-left (bit-and (nth bin (+ o 1)) 0xFF) 8)
          (bit-shift-left (bit-and (nth bin (+ o 2)) 0xFF) 16)
          (bit-shift-left (bit-and (nth bin (+ o 3)) 0xFF) 24)))

(defn read-accessor-f32
  "Read typed data from a glTF accessor in the BIN chunk as a flat f32 vector.
  Supports FLOAT, UNSIGNED_SHORT, UNSIGNED_INT, UNSIGNED_BYTE component types."
  [doc accessor-idx]
  (let [gltf (:gltf doc)
        acc (get (:accessors gltf) accessor-idx)]
    (when-not acc (throw (ex-info "accessor out of range" {:index accessor-idx})))
    (let [bv-idx (:bufferView acc)]
      (when-not bv-idx (throw (ex-info "accessor out of range" {:index accessor-idx})))
      (let [bv (get (:bufferViews gltf) bv-idx)]
        (when-not bv (throw (ex-info "buffer view out of range" {:index bv-idx})))
        (let [components (case (:type acc) "SCALAR" 1 "VEC2" 2 "VEC3" 3 "VEC4" 4 "MAT4" 16 1)
              elem-size (cond
                          (= (:componentType acc) gt/component-type-float) 4
                          (= (:componentType acc) gt/component-type-unsigned-short) 2
                          (= (:componentType acc) gt/component-type-unsigned-byte) 1
                          (= (:componentType acc) gt/component-type-unsigned-int) 4
                          :else 4)
              default-stride (* components elem-size)
              stride (or (:byteStride bv) default-stride)
              base (+ (or (:byteOffset bv) 0) (or (:byteOffset acc) 0))
              bin (:bin doc)]
          (vec
           (for [i (range (:count acc)) c (range components)]
             (let [o (+ base (* i stride) (* c elem-size))]
               (cond
                 (= (:componentType acc) gt/component-type-float)
                 (do (when (> (+ o 4) (count bin)) (throw (ex-info "invalid GLB: accessor data truncated" {})))
                     (read-f32-le bin o))
                 (= (:componentType acc) gt/component-type-unsigned-short)
                 (do (when (> (+ o 2) (count bin)) (throw (ex-info "invalid GLB: accessor data truncated" {})))
                     (double (read-u16-le bin o)))
                 (= (:componentType acc) gt/component-type-unsigned-byte)
                 (do (when (> (+ o 1) (count bin)) (throw (ex-info "invalid GLB: accessor data truncated" {})))
                     (double (bit-and (nth bin o) 0xFF)))
                 (= (:componentType acc) gt/component-type-unsigned-int)
                 (do (when (> (+ o 4) (count bin)) (throw (ex-info "invalid GLB: accessor data truncated" {})))
                     (double (bit-and (read-u32-le bin o) 0xFFFFFFFF)))
                 :else 0.0)))))))))

(defn extract-primitive-mesh
  "Extract interleaved vertex data (pos3+norm3+uv2 = 8 floats/vertex) from a
  VRM mesh primitive. Returns `{:vertices [f32 ...] :indices [u32 ...]}`
  ready for kami-render upload."
  [doc mesh-idx prim-idx]
  (let [mesh (get (:meshes (:gltf doc)) mesh-idx)]
    (when-not mesh (throw (ex-info "part error: mesh not found" {:mesh mesh-idx})))
    (let [prim (get (:primitives mesh) prim-idx)]
      (when-not prim (throw (ex-info "part error: primitive not found" {:primitive prim-idx})))
      (let [pos-acc (get-in prim [:attributes :POSITION])]
        (when-not pos-acc (throw (ex-info "part error: missing POSITION attribute" {})))
        (let [positions (read-accessor-f32 doc pos-acc)
              normals (if-let [norm-acc (get-in prim [:attributes :NORMAL])]
                        (read-accessor-f32 doc norm-acc)
                        (vec (mapcat identity (repeat (/ (count positions) 3) [0.0 1.0 0.0]))))
              uvs (if-let [uv-acc (get-in prim [:attributes :TEXCOORD_0])]
                    (read-accessor-f32 doc uv-acc)
                    (vec (repeat (* (/ (count positions) 3) 2) 0.0)))
              vertex-count (/ (count positions) 3)
              vertices (vec (mapcat (fn [i]
                                       [(nth positions (* i 3)) (nth positions (+ (* i 3) 1)) (nth positions (+ (* i 3) 2))
                                        (nth normals (* i 3)) (nth normals (+ (* i 3) 1)) (nth normals (+ (* i 3) 2))
                                        (nth uvs (* i 2)) (nth uvs (+ (* i 2) 1))])
                                     (range vertex-count)))
              indices (if-let [idx-acc (:indices prim)]
                        (mapv long (read-accessor-f32 doc idx-acc))
                        (vec (range vertex-count)))]
          {:vertices vertices :indices indices})))))
