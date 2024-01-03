## Initial thoughts
Off the bat, this feels like I can just do polynomial interpolation. I'll give it a shot using the formula for [Lagrange polynomials] (https://en.wikipedia.org/wiki/Polynomial_interpolation#Lagrange_Interpolation). 
## Finished part 1, on plane
I finished part 1 on the plane, so I'm unable to submit. The test input matches the spec, and running on the real input, I get -5255931.
## Finished part 1
My initial idea was on the right track, but I got slapped in the face by OCaml's integer overflows, which had me stumped for a while. In the end, I optimized how I was calculating n_choose_k with some algebra.
## Part 2 initial thoughts
This feels fairly straight forward. I just need to compute p(-1) instead of p(n).