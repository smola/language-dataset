use lang.std
system open lang.thread.GC
system open lang.vm.VM
system set memory(1024*1024)

null class Parent:
    def send(int a,int b) = a+b
    def null get(int a);
class Child < Parent:
    @Override
    def get(int a):
        say a
        say send(1,2)
        send(1,2)
    @Override
    def send(int i,int j) = base.send(i,j) + 1

    def modify(int& i):
        i = 10
Parent a = Child
b = a
say a.get(1)
a = Socket("127.0.0.1",10999)
a.open() throws IOException
const PI = 3.14
S = PI*10
a.modify(&S)
arr={1,2,3,4}
map={1:2,2:3,4:5}

