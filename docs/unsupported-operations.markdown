---
layout: page
title: Unsupported Floorplan Operations
permalink: /unsupported-operations/
---

Many public functions and interfaces made available through the `flp-framework`
crate is intrinsically unsafe, and unsupported for direct use in memory manager
implementations. They are made available because:

1. Code generated by the `flp-compiler` crate needs common access to basic
address computations.
2. To provide quick and easy prototyping of Floorplan extensions currently
in development without requiring major modifications to the compiler.
3. To provide an escape hatch for performance sensitive memory management code
which is not well supported by Floorplan.

