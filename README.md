# Raycaster


This is a simple raycaster written in Haskell. It is currently very slow and produces very noisy results unless it generates a high amount of samples, which takes very long time (~10 min for the Out.png picture).

It's now multithreaded, but still *very* slow. It currently takes ~1 hour to generate a 600x400 image with 40 samples:

![3D image of a cube](https://github.com/loovjo/Raycaster-hs/blob/master/Out.png)

To use it, you can run the `Raycaster-exe` file, which will generate the image above.

Currently working on making it faster
