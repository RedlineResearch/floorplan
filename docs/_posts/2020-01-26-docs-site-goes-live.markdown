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

Check back here later for updates on the availability of a crate on [crates.io](https://crates.io).

# The "Hello World" of Floorplan

The simplest memory layout you can define in Floorplan is an opaque chunk of memory,
such as the ``Hello`` type as follows:

{% highlight floorplan %}
Hello -> 1 bytes
{% endhighlight %}

[preprint-link]: https://cronburg.com/papers/floorplan19.pdf
