## Инструментарий для работы с гипперграфами

Гипперграфы реализованы как модификация концепции индуктивных графов Martin Erwig. Как и в оригинальной работе граф строится по индукции:

* Data HyperGraph a b h = (Graph a b, HContext)
* Data Graph a b = Empty | (Context a b) & HyperGraph a b h
* type Context a b h = (Adj b, Adj b, a, Hadj h, Hadj h)
* type Adj b = [(b,Int)]
* type Hadj h = [(h,Int)]
* type HContext h = [([Int], (h,Int), [Int])]

Гиперграф представлен в виде соответствующего изоморфного двудольного графа. Расширение понятия контекста позволяет создавать одновременно как мульти, так и бинарные отношения на множестве вершин.     
