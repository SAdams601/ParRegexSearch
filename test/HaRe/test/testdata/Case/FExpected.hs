module F where
-- Test for refactor of if to case

foo x = case (odd x) of
          True  -> do
            bob x 1
          False -> do
            bob x 2

bob x y = x + y
