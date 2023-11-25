import MyVector

testFromList = vecFromList [1, 2, 3]
testScalar = vecScalar 2 (vecFromList [1, 2, 3])
testAdd = vecAdd testFromList testScalar
testProd = vecProd testFromList testScalar
testCross = crossProd (Vector [1, 2, 3]) (Vector [-7, 8, 9])
