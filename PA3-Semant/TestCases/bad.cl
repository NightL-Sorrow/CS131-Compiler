class A{
    b:SELF_TYPE;
    -- x:Bool<- (let x:Int <-"a" in x=x);
    a(x:Int,y:Int):SELF_TYPE{b};
};
class Main INHERITS A {
    main():Int{1};
};
class C{

};
class B INHERITS A{
    m:SELF_TYPE;
    a(x:Int,y:Int):Object{m@Z.a(1,1)};
};
