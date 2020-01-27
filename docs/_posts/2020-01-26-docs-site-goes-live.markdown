---
layout: post
title:  "Floorplan Compiler Documentation"
date:   2020-01-26 15:50:00 -0500
categories: floorplan update
---

The most extensive documentation of the compiler can at present be
found in the preprint of our accompanying conference paper [as available here][preprint-link].
For prospective users of the compiler, we recommend reading Section 3 of the paper as that
provides a practical view of what type of code the compiler generates for you.

# Installation

The Floorplan compiler itself (Haskell project) must be installed from source
with the `stack` toolchain like so:

```bash
git clone https://github.com/RedlineResearch/floorplan.git
cd floorplan
stack build
stack install
```

Once this is done, and the `flp` executable from doing so is on your `$PATH`, you
can write Floorplan-enabled Rust crates by adding the following two crates to
your `Cargo.toml`:

- [flp-framework-0.1.0](https://crates.io/crates/flp-framework/0.1.0) added to `[dependencies]`
- [flp-compiler-0.1.0](https://crates.io/crates/flp-compiler/0.1.0) added to `[build-dependencies]`

# The "Hello World" of Floorplan

The simplest memory layout you can define in Floorplan is an opaque chunk of memory,
such as the ``Hello`` type as follows:

{% highlight floorplan %}
Hello -> 1 bytes
{% endhighlight %}

[preprint-link]: https://cronburg.com/papers/floorplan19.pdf
