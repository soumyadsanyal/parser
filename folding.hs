

leftfold :: (valtype -> listtype -> valtype) -> valtype  -> [listtype] -> valtype
leftfold function value [] = value
leftfold function value (first:rest) = 
 leftfold function (function value first) rest


rightfold :: (listtype -> valtype -> valtype) -> valtype -> [listtype] -> valtype
rightfold function value [] = value
rightfold function value (first:rest) =
 function first (rightfold function value rest)

strictleftfold :: (valtype -> listtype -> valtype) -> valtype -> [listtype] -> valtype
strictleftfold function value [] = value
strictleftfold function value (first:rest) = 
 seq (function value first) $ strictleftfold function (function value first) rest
