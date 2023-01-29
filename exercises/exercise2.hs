import qualified System.Random as R

binary_random_response :: R.StdGen -> Bool -> (Bool, R.StdGen)
binary_random_response g true_answer =
  let (first_coin, g') = R.random g
  in if first_coin then (true_answer, g')
     else R.random g'