! 离轴系统交点计算宏 
!	call_jmrcc.zpl 调用宏
!
! 参数列表
! 线1: 1: 	面号； 2-5: hx,hy,px,py
! 点1: 6: 	面号； 7-10: hx,hy,px,py
! 线2: 11: 	面号； 12-15: hx,hy,px,py
!		如果11为0，则线2不生效，计算第一类间距
!		如果11为-1，则计算线1 路径上 线1所在面 到 点1 所在面 间的直线距离
!		如果11为其他，则计算第二类间距
!	
! 结果: 16                                                 
!                  by: H_H 
!				2017/11/3                                   

!  输出 头部
PRINT 
PRINT 
PRINT "             离轴系统交点计算宏"
PRINT 
PRINT "                    H_H  "
PRINT 
PRINT "--------------------------------------------"
PRINT 
PRINT "描述:"
PRINT "     计算 线1 到 点1 的Y方向间距"
PRINT "     计算 线1 到 (线2 与线3 交点） 的Y方向间距"
PRINT "     计算 某光线路径上，任意两面交点间的距离"
PRINT "输入："
PRINT "     线1 面号,hx,hy,px,py"
PRINT "     点2 面号,hx,hy,px,py"
PRINT "     线1 面号,hx,hy,px,py"
PRINT "输出："
PRINT "     间距"
PRINT 
PRINT "--------------------------------------------"
PRINT 
! 见证奇迹的时刻

BEEP

! 初始化数据

! 内存结构 
!
! 线1: 1: 	面号 2-5: hx,hy,px,py
! 点1: 6: 	面号 7-10: hx,hy,px,py
! 线2: 11: 	面号 12-15: hx,hy,px,py
!		如果11为0，则线2不生效，计算第一类间距
!		如果11为-1，则计算线1 路径上 线1所在面 到 点1 所在面 间的直线距离
!		
! 结果: 16

! 向缓存填坑

CALLSETDBL 1, 3
CALLSETDBL 2, 0
CALLSETDBL 3, 1
CALLSETDBL 4, 0
CALLSETDBL 5, 1

CALLSETDBL 6, 12
CALLSETDBL 7, 0
CALLSETDBL 8, -1
CALLSETDBL 9, 0
CALLSETDBL 10, 0

CALLSETDBL 11, 0
CALLSETDBL 12, 0
CALLSETDBL 13, 0
CALLSETDBL 14, 0
CALLSETDBL 15, 0

CALLSETDBL 16, 0

! 调用结果
CALLMACRO jmrcc.ZPL

disY = CALD(16)

! 如果还有计算，把上面复制下来再计算一遍，将结果累加

PRINT 
PRINT "间距Y:", disY



