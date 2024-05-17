(* Class definition, redefinition, and inheritance errors

class AA inherits SELF_TYPE {

};

class AB inherits Int {

};

class AC inherits String {

};

class AD inherits Bool {

};

class Object {

};

class Int {

};

class SELF_TYPE {

};

class A {

};

class A{

};

*)

class AE inherits A {

};

class A inherits AE {

};

class P {

};

(* class C inherits A {
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

class C {

};

class B inherits D {

};

class A inherits B {

};




Class Main {
	main():C {
	 {
	  (new C).init(1,1);
	  (new C).init(1,true,3);
	  (new C).iinit(1,true);
	  (new C);
	 }
	};
};

*)
