import Data.List
import Data.Function

main = do
  let hs = 165 :: Double
      f = 55
      s = 55
      t = hs - f -s
      gamma = 0*degree
      b = 45.0 / 2
      a = 63
      hs0 = a+b
      alpha = 75*degree
      tf beta' = sin(alpha)/(cos(gamma)*sin(beta'))
      hs1 = cos(beta)*t+cos(alpha)*f+s
      beta = minimumBy (compare `on` (\x -> abs ((tf x) - 1))) (fmap (*degree) [0,0.01..90])
  print (tf beta,beta/degree,hs1,hs0)

degree = pi/180
