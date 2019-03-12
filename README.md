# A simple signed distance function renderer

This simple stand-alone demo illustrates the basics of both the sphere tracing method of ray marching and constructive solid geometry modelling with signed distance fields.

The project is built from scratch without any external dependencies; the program itself is less than six hundred lines of functional Scala in its entirety.  The renderer is written specifically to run exclusively on the CPU instead of the GPU so as to best illustate the entire rendering pipeline as clearly and concisely as possible.

When run, the program produces this image (writing it to disk in PNG format):

![composite](/renders/render-00-composite.png)

To quickly run this program, install `sbt` (the in interactive Scala build tool), clone this reposititory and then from the root of the repository run:

```
sbt run
```

