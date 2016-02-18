# portfolio-sim-nss
Constant Correlation Model without Short Sale

The Constant Correlation short sales not allowed function works the same
way as the one with short sales allowed. The only difference is that we get weights
assuming there is no short sales opportunities available. In our case, we type the
following:

port.cc.nss(data=ret.mon,rf=0.001)
