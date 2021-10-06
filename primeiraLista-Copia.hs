import Data.Char
-- Aula: Tipos Básicos e Definições

-- 1
tresIguais :: Int -> Int -> Int -> Bool
tresIguais a b c = (a==b)&&(a==c)
--a
quatroIguais :: Int -> Int -> Int -> Int -> Bool
quatroIguais a b c d = ((a==b)&&(b==c)&&(c==d))
--b
quatroIguaisb :: Int -> Int -> Int -> Int -> Bool
quatroIguaisb a b c d = ((tresIguais a b c)&&(tresIguais a b d))
--c.1
quatroIguaisc :: Int -> Int -> Int -> Int -> Bool
quatroIguaisc a b c d = iguais
 where iguais = ((a==b)&&(b==c)&&(c==d))
--c.2
quatroIguaisd :: Int -> Int -> Int -> Int -> Bool
quatroIguaisd a b c d = let iguais = ((a==b)&&(b==c)&&(c==d)) in iguais
--d
{- casos de teste:
1- todos iguais = True, Ex: 1 1 1 1
2- um ou mais valores diferentes dentre os quatro = False, EX: 1 1 1 2
-}

-- 2
--a
tresDifa :: Int -> Int -> Int -> Bool
tresDifa a b c = (a/=b)&&(b/=c)&&(a/=c)
--b
quantosIguais :: Int -> Int -> Int -> Int
quantosIguais a b c 
 |(tresIguais a b c == True) = 3
 |(tresDifa a b c == True) = 0
 |otherwise = 2
--c
quantosIguais4 :: Int -> Int -> Int -> Int -> Int
quantosIguais4 a b c d
 |((quantosIguais a b c)==3)&&(d==a) = 4
 |(quantosIguais a b c == 3)||(quantosIguais a b d == 3)||(quantosIguais a c d == 3)||(quantosIguais b c d == 3) = 3
 |(quantosIguais a b c == 2)||(quantosIguais a b d == 2)||(quantosIguais c b d == 2)||(quantosIguais a c d==2) = 2
 |otherwise = 0
--d
 {- casos de teste:
-tresDifa :
1- 3 diferentes = True,ex: 1 2 3
2- se qualquer valor se repete = False, ex: 1 1 2
-quantosIguais:
1- todos Iguais = 3,ex: 3 3 3
2- dois Iguais = 2,ex: 3 3 2
3- todos diferentes = 0,ex: 3 2 1
-quantosIguais4:
1- 4 iguais = 4, ex: 4 4 4 4
2- 3 iguais = 3, ex 4 4 4 3
3- 2 iguais = 2, ex: 4 4 3 3, 4 4 2 3
4- todos diferentes = 0, ex: 4 3 2 1
-}

-- Aula: Tuplas e Listas

-- 3
type Codigo = Int
type Nome = String
type Preco = Int
type ItemRest = (Codigo, Nome, Preco)
type Menu = [ItemRest]

cardapio :: Menu
cardapio = [(150, "Pastel", 500),(15, "Agua", 200),(2, "Cerveja", 400), (40, "Picanha", 4500),(52, "Pudim", 1200),(10,"Coxinha",500),(5,"filé",100),(4,"Strognoff",1000),(1,"Pinga",100),(12,"lasanha",700),(44,"torta doce",600),(9,"feijoada",2000),(55,"buxada",5000),(3,"cafe",200)]

type Mesa = Int
type Quant= Int
type ItemCliente = (Codigo, Quant)
type PedidoCliente = [ItemCliente]
type PedidosMesas = [PedidoCliente]

pedidosRest :: PedidosMesas
pedidosRest = [[(150,1),(2,2)], [], [], [(40,1),(2,2),(52,2)], []]

infixl 7 //
(//):: Int -> Int -> Float
a // b = fromIntegral a/fromIntegral b

-- 3.1

--a
itemNaLista :: Menu -> ItemRest -> Bool
itemNaLista c (f,g,h) = (f==head[f|(j,k,l) <- c,(f==j)])

adicionaItemMenu :: Menu -> ItemRest -> Menu
adicionaItemMenu c a = if (itemNaLista c a==True) then error "Codigo ja cadastrado" else a:c

--b
codigoNaLista :: Menu -> Codigo -> Bool
codigoNaLista a b = ([b]==[codigo|(codigo,nome,preco) <- a, codigo==b])

removeItemMenu :: Menu -> Codigo -> Menu
removeItemMenu a b = if (codigoNaLista a b ==False) then error "Código inexistente no menu" else [(codigo,nome,preco)|(codigo,nome,preco) <- a, codigo/=b]

--c
coletaItemMenu :: Menu -> Codigo -> ItemRest
coletaItemMenu a b = head [(codigo,nome,preco)|(codigo,nome,preco) <- a, codigo==b]

--3.2

--a
indexaLista :: [(Int,Int)] -> [(Int,(Int,Int))]
indexaLista a = zip ps a
 where ps = [1..(length a)]
 
encontraPosicao :: (Int,Int) -> [(Int,(Int,Int))] -> Int
encontraPosicao (x,y) xs = head px
 where px = [p|(p,(g,k))<-xs,g==x]
 
atualiza :: Int -> (Int,Int) -> PedidoCliente -> [(Int,Int)]
atualiza m (a,b) xs = take ((encontraPosicao (a,b) (indexaLista xs))-1) xs++kk++drop ((encontraPosicao (a,b) (indexaLista xs))) xs
 where kk = [(f,(g+b))|(f,g)<-xs,f==a]

atualiza1 :: Int -> (Int,Int) -> PedidoCliente -> [(Int,Int)]
atualiza1 m (a,b) xs = take ((encontraPosicao (a,b) (indexaLista xs))-1) xs++kk++drop ((encontraPosicao (a,b) (indexaLista xs))) xs
 where kk = [(f,(g-b))|(f,g)<-xs,f==a]
 
cancela ::(Int,Int) -> PedidoCliente -> [(Int,Int)]
cancela (a,b) xs = take ((encontraPosicao (a,b) (indexaLista xs))-1) xs++drop ((encontraPosicao (a,b) (indexaLista xs))) xs
 
 
adicionaPedido :: Mesa -> ItemCliente -> PedidosMesas -> PedidosMesas
adicionaPedido a (x,y) xs
  |(mesaVazia==True)=(take (a-1) xs)++[(x,y):xs!!(a-1)]++(drop a xs)
  |(pedidojaExiste/=[x])=(take (a-1) xs)++[(x,y):xs!!(a-1)]++(drop a xs)
  |(pedidojaExiste==[x])=(take (a-1) xs)++[atualiza a (x,y) (xs!!(a-1))]++(drop a xs)
  |otherwise  = error "Mesa inexistente"
  where mesaVazia=[]==xs!!(a-1)
        pedidojaExiste=[f|(f,g)<-(xs!!(a-1)),f==x]

--b
cancelaPedido :: Mesa -> ItemCliente -> PedidosMesas -> PedidosMesas
cancelaPedido a (x,y) xs 
 |quantidadeIgual==True= (take (a-1) xs)++[cancela (x,y) (xs!!(a-1))]++(drop a xs)
 |quantidadeIgual==False= (take (a-1) xs)++[atualiza1 a (x,y) (xs!!(a-1))]++(drop a xs)
 |otherwise = error "algo errado"
  where quantidadeIgual = ([(x,y)]==[(b,c)|(b,c)<-(xs!!(a-1)),b==x,c==y])
  
--c
pedidoCompletoMesa :: Mesa -> PedidosMesas -> Menu -> [(Quant, Nome, Preco)]
pedidoCompletoMesa a xs ys = [(q,n,p*q)|(c,q)<-xs!!(a-1),(l,n,p)<-ys,c==l]

--d 
totalMesa :: [(Quant, Nome, Preco)] -> Preco
totalMesa xs = sum[p|(q,n,p)<-xs]

-- 3.3
--a
formataPreco :: Preco -> String
formataPreco a = take (6-(length (show (a // 100)))) "....."++show (a // 100)++"0"

--b
formataLinha :: (Quant,Nome,Preco) -> String
formataLinha (a,b,c) = show (a)++" "++b++replicate (30-(length b)) '.'++formataPreco c++"\n"

--c
formataLinhas :: [(Quant,Nome,Preco)] -> String
formataLinhas xs = (concat[(formataLinha (a,b,c))|(a,b,c)<-xs])

--d
formataTotal:: [(Quant,Nome,Preco)] -> String
formataTotal xs = "\n"++"  "++"Total"++replicate 25 '.'++formataPreco (totalMesa xs)

--e
geraConta:: Mesa -> PedidosMesas -> Menu -> IO()
geraConta a xs ys = putStr (formataLinhas (pedidoCompletoMesa a xs ys)++formataTotal (pedidoCompletoMesa a xs ys))

--f
liberaMesa:: Mesa -> PedidosMesas -> PedidosMesas
liberaMesa a xs = take(a-1) xs++[[]]++drop a xs
--Aula: Projeto e Escrita de Programas e Construção de Algoritmos por Indução Fraca e Forte

--4.1
--a)
codigoMenu :: Menu -> ItemRest -> Bool
codigoMenu [] _ = False
codigoMenu ((z,x,v):xs) (a,b,c)
 |z==a=True
 |otherwise= codigoMenu xs (a,b,c)
 
adicionaItemMenu2 :: Menu -> ItemRest -> Menu
adicionaItemMenu2 ((z,x,v):xs) (a,b,c) = if 
  (codigoMenu ((z,x,v):xs) (a,b,c)==True) then error "codigo ja esta no menu" else (a,b,c):((z,x,v):xs) 

--b)
codigoMenu2 :: Menu -> Codigo -> Bool
codigoMenu2 [] _ = False
codigoMenu2 ((z,x,v):xs) a
 |z==a=True
 |otherwise= codigoMenu2 xs a
 
removeItem :: Menu -> Codigo -> Menu
removeItem [] _ = []
removeItem ((z,x,v):xs) a
 |z==a= removeItem xs a
 |otherwise= (z,x,v):removeItem xs a
 
removeItemMenu2 :: Menu -> Codigo -> Menu
removeItemMenu2 [] _ = []
removeItemMenu2 xs a = if (codigoMenu2 xs a==False) then error "codigo nao esta na lista" else removeItem xs a

--c)
coletaItemMenu2 :: Menu -> Codigo -> ItemRest
coletaItemMenu2 [] _ = error "codigo nao encontrado"
coletaItemMenu2 ((z,x,v):xs) a 
 |z==a= (z,x,v)
 |otherwise = coletaItemMenu2 xs a

--4.2
--a)
codigoExistente :: ItemCliente -> [ItemCliente] -> Bool
codigoExistente _ [] = False
codigoExistente (b,c) (x:xs)
 |b==fst x=True
 |otherwise= codigoExistente (b,c) xs
 
atualizaValor2 :: ItemCliente -> [ItemCliente] -> [ItemCliente]
atualizaValor2 _ [] = []
atualizaValor2 (b,c) ((z,x):xs)
 |b==z=(z,x+c):atualizaValor2 (b,c) xs
 |otherwise= (z,x):atualizaValor2 (b,c) xs

adicionaPedido2 :: Mesa -> ItemCliente -> PedidosMesas -> PedidosMesas
adicionaPedido2 a y xss
 |(codigoExistente y (xss!!(a-1)))==True= take (a-1) xss++[atualizaValor2 y (xss!!(a-1))]++drop a xss
 |otherwise= take (a-1) xss++[y:xss!!(a-1)]++drop a xss
 
--b)
cancela2 :: ItemCliente -> [ItemCliente] -> [ItemCliente]
cancela2 _ [] = []
cancela2 (b,c) ((z,x):xs)
 |(b==z)&&(c==x)= cancela2 (b,c) xs
 |(b==z)&&(x>c)= (z,x-c):cancela2 (b,c) xs
 |otherwise= (z,x):cancela2 (b,c) xs

cancelaPedido2 :: Mesa -> ItemCliente -> PedidosMesas -> PedidosMesas
cancelaPedido2 a y xss = take (a-1) xss++[cancela2 y (xss!!(a-1))]++drop a xss

--c)
intercala1:: ItemCliente -> Menu -> (Quant, Nome, Preco)
intercala1 (x,y) ((a,b,c):xs)
 |x==a= (y,b,c*y)
 |otherwise= intercala1 (x,y) xs

intercala:: PedidoCliente -> Menu -> [(Quant, Nome, Preco)]
intercala [] _ = []
intercala (x:xs) ys = intercala1 x ys:intercala xs ys

pedidoCompletoMesa2 :: Mesa -> PedidosMesas -> Menu -> [(Quant, Nome, Preco)]
pedidoCompletoMesa2 a xs ys = intercala (xs!!(a-1)) ys

--d) 
totalMesa2 :: [(Quant, Nome, Preco)] -> Preco
totalMesa2 [] = 0
totalMesa2 ((a,b,c):xs) = c+totalMesa2 xs



 


















 
 
 




