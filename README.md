ZICA_ML
=======

Z Instant Comprehensive ABAP - Machine Learning Library

In 2014 I started to learn about Machine Learning using Octave. It was nice, but been an ABAP developer for the last 10 years made me ask: can I do this using ABAP?

You know, some people say that ABAP is a small, weak, script language that can be used for small code changes and customizing. But we know that it is quite the opposite.

And here it is. The project is organized as described bellow.

**ZICA_ML.ABAP:**

This is the main library include. It contains the basic type declaration and basic routines to implement a machine learning algorithm.

**LIREG folder:**

It contains an example of the Linear Regression algorithm using the ZICA_ML.ABAP include library. This algorithm is based on the lectures of the Stanford Machine Learning course, by Andrew Ng. It tries to predict the price of a pen given some characteristics. It uses training data LIREG_MULTI_X.TXT and LIREG_MULTI_Y.TXT to generage THETA parameters that can be used to predict the price based on your own input parameters.

**NN folder:**

This example implements a neural network that tries to predict a number inside a 20x20 pixel bitmap, from 0 to 9. It is also based on the lecturs of the Stanford Machine Learning course, by Andrew Ng. It uses a "pre-learned" THETA parameter set, and tries to predict the number of the image that you choose. Hint: you can try to generate new images with numbers from different fonts and handwritten styles and see if the program can understand those too.

