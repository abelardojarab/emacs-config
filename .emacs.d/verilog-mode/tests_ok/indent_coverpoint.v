
// here is the same code with verilog-mode indenting
// covergroup example from IEEE pg. 317
covergroup cg @(posedge clk);
   a: coverpoint v_a {
      bins a1 = { [0:63] };
      bins a2 = { [64:127] };
      bins a3 = { [128:191] };
      bins a4 = { [192:255] };
   }
   b: coverpoint v_b iff ( g )
     {
      bins b1 = {0};
      bins b2 = { [1:84] };
      bins b3 = { [85:169] };
      bins b4 = { [170:255] };
   }
   c: coverpoint v_c iff ( !g ) {
      bins c1 = {0};
      bins c2 = { [1:84] };
      bins c3 = { [85:169] };
      bins c4 = { [170:255] };
   }
   d: cross a , b, c {
      bins c1 = ! binsof(a) intersect {[100:200]}; // 4 cross products
      bins c2 = binsof(a.a2) || binsof(b.b2); // 7 cross products
      bins c3 = binsof(a.a1) && binsof(b.b4); // 1 cross product
   }
endgroup
