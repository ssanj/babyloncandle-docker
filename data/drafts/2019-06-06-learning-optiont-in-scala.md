---
title: Learning OptionT in Scala
author: sanjiv sahayam
description: ???
tags: scala
comments: true
---

case class OptionT[F[_], A](value: F[Option[A]])

- Lift an Option into OptionT
 - Some
    ```
    OptionT.fromOption[List](Some(1))
    ```

 - None
  ```
  OptionT.none[List, Int]
  ```

- Lift an A into OptionT
   ```
  Option.some[Int](1)
  OptionT.pure[List](1)
  ```
- List an F[A] into OptionT
  ```
  OptionT.liftF[List, Int](List(1))
  ```

- Lift an F[Option[A]] into OptionT
  ```
  OptionT.apply[List, Int](List(Some(1)))
  ```
- map on A
```
val op1 = OptionT.fromOption[List](Some(1))
op1.map(_ + 1)
> res11: cats.data.OptionT[List,Int] = OptionT(List(Some(2)))
```
- map on Option[A]
```
scala> op1.transform(op => op.map(_ => 10))
res13: cats.data.OptionT[List,Int] = OptionT(List(Some(10)))

scala> op1.transform(_ => (None:Option[Int]))
res15: cats.data.OptionT[List,Int] = OptionT(List(None))
```

- map on F[Option[A]] 

# Functions
- fold
- flatMap
- flatMapF
- transform
- flatTransform
- getOrElse
- getOrElseF
- orElse
- orElseF
- semiflatMap
- subflatMap
- mapK
- mapFilter
- toLeft
- toRight
- traverse
- toNested