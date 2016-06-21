import me.d_d.delaying.ResizableArray

val da = ResizableArray(0 until 32 : _*)
val t = da.updated(0, 666)
t(0)
t(1)
t(2)

