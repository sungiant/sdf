# A simple software signed distance function renderer

This simple demo illustrates the basics of both ray marching and modelling with signed distance functions, it has no dependencies and is just over two hundred lines of pure functional Scala.  The renderer is written specifically to run eclusively on the CPU instead of the GPU so as to best illustate the entire rendering pipeline as clearly and concisely as possible.

When run the program produces this image, writing it to disk in PPM format:

![preview](https://raw.githubusercontent.com/sungiant/sdf/master/preview.png)

To quickly run this program, install `sbt` (the in interactive Scala build tool), clone this reposititory and then from the root of the repository run:

```
sbt run
```

