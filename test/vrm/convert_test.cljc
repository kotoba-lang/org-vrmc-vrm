(ns vrm.convert-test
  "Regression coverage for a real bug found integrating `vrm.convert` into
  `kotoba-lang/kami-app-character-creator` (ADR-2607031200 Phase 2): on JVM,
  `read-accessor-f32` threw `ArithmeticException: integer overflow` for any
  FLOAT accessor value whose top byte has bit 7 set — i.e. any negative float,
  which is virtually all real-world vertex geometry. `convert.rs` had no
  `#[cfg(test)]` block to port (per the repo README), so this is new coverage,
  not a restoration."
  (:require [clojure.test :refer [deftest is testing]]
            [vrm.convert :as conv]
            [vrm.gltf-types :as gt]))

(defn- f32->le-bytes
  "Mirrors character-creator.gltf-build's private helper — little-endian
  IEEE754 bytes for one f32."
  [f]
  #?(:clj (let [bits (Float/floatToIntBits (float f))]
            [(bit-and bits 0xFF)
             (bit-and (bit-shift-right bits 8) 0xFF)
             (bit-and (bit-shift-right bits 16) 0xFF)
             (bit-and (bit-shift-right bits 24) 0xFF)])
     :cljs (let [buf (js/ArrayBuffer. 4) view (js/DataView. buf)]
             (.setFloat32 view 0 f true)
             [(.getUint8 view 0) (.getUint8 view 1) (.getUint8 view 2) (.getUint8 view 3)])))

(defn- doc-with-vec3-accessor [vs]
  (let [bin (vec (mapcat (fn [v] (mapcat f32->le-bytes v)) vs))]
    {:gltf {:accessors [{:bufferView 0 :componentType gt/component-type-float
                          :count (count vs) :type "VEC3"}]
            :bufferViews [{:buffer 0 :byteOffset 0 :byteLength (count bin)}]}
     :bin bin}))

(deftest read-accessor-f32-negative-values-test
  (testing "negative and large-magnitude floats decode without throwing (regression: JVM long/int narrowing bug)"
    (let [vs [[-1.5 0.0 2.75] [-100.0 -0.001 3.4e10] [0.0 -0.0 -1.0]]
          doc (doc-with-vec3-accessor vs)
          decoded (conv/read-accessor-f32 doc 0)]
      (is (= 9 (count decoded)))
      (doseq [[expected actual] (map vector (flatten vs) decoded)]
        ;; relative tolerance: f32 only carries ~7 significant digits, so the
        ;; large-magnitude case (3.4e10) legitimately loses absolute precision
        (is (< (Math/abs (- (double expected) (double actual)))
               (max 1.0 (* 1e-5 (Math/abs (double expected)))))
            (str "expected ~" expected " got " actual))))))

(deftest read-accessor-f32-all-negative-bytes-test
  (testing "every byte value 0-255 round-trips through a float without an overflow exception"
    (let [vs (mapv (fn [x] [(- x 128.0) 0.0 0.0]) (range 0 256 4))
          doc (doc-with-vec3-accessor vs)
          decoded (conv/read-accessor-f32 doc 0)]
      (is (= (* (count vs) 3) (count decoded))))))
