-- Prática 02 de Haskell
-- Nome: Izabella M. Paulette

--1)Médicos consideram que um indivíduo tem febre quando sua temperatura corpórea está acima de 37,8oC. Escreva uma função comFebre :: [Float] -> [Float] que, dada uma lista de temperaturas de indivíduos, selecione aquelas que representam febre. Resolva esta questão definindo uma função auxiliar nomeada, que verifica se uma dada temperatura é febre ou não.
comFebre :: [Float] -> [Float]
febre :: Float -> Bool
febre temp = temp > 37.8
comFebre temps = filter febre temps

--2)Escreva uma função comFebre' :: [Float] -> [Float] que resolva a questão anterior usando lambda.
comFebre' ::[Float] -> [Float]
comFebre' temps' = filter (\x -> x>37.8) temps'

--3)Crie uma função itemize :: [String] -> [String] que receba uma lista de strings e adicione tags HTML <li> e </ li> antes e depois de cada string. Resolva esta questão usando lambda.
itemize :: [String] -> [String]
itemize lis = map (\x -> "<li> " ++ x ++ " </li>") lis

--4)Escreva uma função bigCircles :: Float -> [Int] -> [Float] que receba um número e uma lista de raios de círculos. Essa função deverá retornar somente aqueles raios de círculos cuja área seja maior que o número passado como argumento.
bigCircles :: Float -> [Float] -> [Float]
bigCircles a r = filter (\x -> (pi * x ^ 2) > a) r

--5)Escreva uma função quarentena :: [(String,Float)] -> [(String,Float)] que receba uma lista de tuplas com nomes de pessoas e suas temperaturas corpóreas, e selecione aquelas que têm febre.
quarentena :: [(String,Float)] -> [(String,Float)]
quarentena tupla = filter (\(_,temp) -> (febre temp)) tupla

--6)Escreva uma função idadesEm :: [Int] -> Int -> [Int] que receba uma lista de anos de nascimento de algumas pessoas e um ano de referência. A lista resultante terá idades calculadas considerando o ano de referência (idades aproximadas, já que só consideram o ano, não a data completa de nascimento). Resolva esta questão usando lambda.
idadesEm :: [Int] -> Int -> [Int]
idadesEm lisA ano = map (\x -> ano - x) lisA

--7)Escreva uma função changeNames :: [String] -> [String] que receba uma lista de nomes e adicione o prefixo "Super " às strings que começarem com a letra A (maiúscula), deixando as demais inalteradas. A lista resultante, portanto, terá a mesma quantidade de strings da lista original.
changeNames :: [String] -> [String]
changeNames nomes = map (\x -> if head x == 'A' then ("Super " ++ x) else x) nomes

--8)Escreva uma função onlyShorts :: [String] -> [String] que receba uma lista de strings e retorne outra lista contendo somente as strings cujo tamanho seja menor que 5.
onlyShorts :: [String] -> [String]
onlyShorts lista = filter (\x -> length x < 5) lista