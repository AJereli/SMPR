# Метрические алгоритмы классификации

Метрический алгоритм - алгоритм классификации основанный на оценке близости объектов, используя функцию расстояния.
При этом функция расстояния не обязательно должна быть метрикой, то есть возможно нарушение неравенства треугольника

При этом метрические алгоритмы основываются на гипотезе компактности, которяа говорит что схожие объекты чаще лежат в одном классе, чем в разных, мера сходства здесь - введенное расстояние.

## Парзеновское окно
Парзеновское окно - метрический алгоритм, частный случай метрических алгоритмов

h - параметр характеризующий ширину окна.

Оптимальное значение ℎ находим скользящим контролем, в частности leave-one-out

Алгоритм применялся для задачи классификации Ирисов Фишера по признакам Petal.Length
и Petal.Width

### Количество ошибок в LOO при соответствующих ядрах
- Епанечникова = 6 
- Квартическое = 6 
- Треуольное = 6 
- Гауссовское = 6
- Прямоугольное = 6

![](https://github.com/AJereli/SMPR/blob/master/grafic.png)

![](figures/img.png)

## kwNN
kwNN - метод взвешенных ближайших соседей, в отличии от kNN, оценивает степень важность каждого объекта обучающей выборки,
используя параметр w - вес объекта, где w = q^i. q - некоторое число ( 0 < q < 1). При q = 1, kwNN выраждается в kNN

Значение q находим методом LOO

![](https://github.com/AJereli/SMPR/blob/master/kwnn_g.png)

![](figures/img.png)
