let p = "(if (>= x 0)
         skip
         (set! x (- 0 x)))"
let whileadd = "(while (> x 0)
          (and (>= x 0) (= (+ x y) (+ a b)))
          (block
           (set! x (- x 1))
          (set! y (+ y 1))))"
