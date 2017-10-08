structure Bool = struct
  val body = "
日本語
の
文章
"
  val f = fn true => 1
          |  false => 2
  val i = 1
  datatype bool = true | false
  datatype 'a option = some of 'a | none
  val value = fn (some x) => true
              |  none   => false
end
