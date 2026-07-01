# kotoba-lang/vrm

Zero-dep portable `.cljc` — restored from the legacy `kami-engine/kami-vrm` Rust crate
(deleted in the kotoba-lang Rust removal) as part of the **clj-wgsl migration** (ADR-2607010930,
`com-junkawasaki/root`).

## Status

Scaffold only — the CLJC restoration is pending. This repo provides the home for the
zero-dep portable `.cljc` contracts / data interpreters / EDN IR that replace the deleted
Rust crate. Native execution (wgpu / wasmtime / wasmi) stays substrate.

## Develop

```bash
clojure -M:test
```
