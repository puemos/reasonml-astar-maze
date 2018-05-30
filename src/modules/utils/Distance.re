let euclideanDistance = ((x1, y1), (x2, y2)) => abs(x2 - x1) + abs(y2 - y1);

/* int_of_float(
     (
       (float_of_int(x1) -. float_of_int(x2))
       ** 2.0
       +. (float_of_int(y1) -. float_of_int(y2))
       ** 2.0
     )
     ** 0.5,
   ); */