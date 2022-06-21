# basic_math_exp_tree
basic cmd calculator demo made in non-recursive way to build an expression tree using modern C++

Designed to show the process of building an expression tree, focused on easy to read/debug/maintain object model.
Not designed to be very effective, but suprisingly - on simplest expressions (with int constants, brackets and +-/* operations) - 
it performs 5x faster (MSVS, /O2) than expertk library (which is considered very optimized).
