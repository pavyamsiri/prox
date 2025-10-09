mod common;

use common::check;

#[test]
fn get_on_num() {
    const CODE: &str = "123.foo; // expect runtime error: Only instances have properties.
    ";
    check(CODE);
}

#[test]
fn get_on_nil() {
    const CODE: &str = "nil.foo; // expect runtime error: Only instances have properties.
    ";
    check(CODE);
}

#[test]
fn set_on_class() {
    const CODE: &str = r#"class Foo {}
Foo.bar = "value"; // expect runtime error: Only instances have fields.
    "#;
    check(CODE);
}

#[test]
fn get_on_string() {
    const CODE: &str = r#""str".foo; // expect runtime error: Only instances have properties.
    "#;
    check(CODE);
}

#[test]
fn call_nonfunction_field() {
    const CODE: &str = r#"class Foo {}

var foo = Foo();
foo.bar = "not fn";

foo.bar(); // expect runtime error: Can only call functions and classes.
    "#;
    check(CODE);
}

#[test]
fn set_on_bool() {
    const CODE: &str = r#"true.foo = "value"; // expect runtime error: Only instances have fields.
    "#;
    check(CODE);
}

#[test]
fn method_binds_this() {
    const CODE: &str = r#"class Foo {
  sayName(a) {
    print this.name;
    print a;
  }
}

var foo1 = Foo();
foo1.name = "foo1";

var foo2 = Foo();
foo2.name = "foo2";

// Store the method reference on another object.
foo2.fn = foo1.sayName;
// Still retains original receiver.
foo2.fn(1);
// expect: foo1
// expect: 1
    "#;
    check(CODE);
}

#[test]
fn set_on_num() {
    const CODE: &str = r#"123.foo = "value"; // expect runtime error: Only instances have fields.
    "#;
    check(CODE);
}

#[expect(clippy::too_many_lines, reason = "this is the point of the test.")]
#[test]
fn many() {
    const CODE: &str = r#"class Foo {}

var foo = Foo();
fun setFields() {
  foo.bilberry = "bilberry";
  foo.lime = "lime";
  foo.elderberry = "elderberry";
  foo.raspberry = "raspberry";
  foo.gooseberry = "gooseberry";
  foo.longan = "longan";
  foo.mandarine = "mandarine";
  foo.kiwifruit = "kiwifruit";
  foo.orange = "orange";
  foo.pomegranate = "pomegranate";
  foo.tomato = "tomato";
  foo.banana = "banana";
  foo.juniper = "juniper";
  foo.damson = "damson";
  foo.blackcurrant = "blackcurrant";
  foo.peach = "peach";
  foo.grape = "grape";
  foo.mango = "mango";
  foo.redcurrant = "redcurrant";
  foo.watermelon = "watermelon";
  foo.plumcot = "plumcot";
  foo.papaya = "papaya";
  foo.cloudberry = "cloudberry";
  foo.rambutan = "rambutan";
  foo.salak = "salak";
  foo.physalis = "physalis";
  foo.huckleberry = "huckleberry";
  foo.coconut = "coconut";
  foo.date = "date";
  foo.tamarind = "tamarind";
  foo.lychee = "lychee";
  foo.raisin = "raisin";
  foo.apple = "apple";
  foo.avocado = "avocado";
  foo.nectarine = "nectarine";
  foo.pomelo = "pomelo";
  foo.melon = "melon";
  foo.currant = "currant";
  foo.plum = "plum";
  foo.persimmon = "persimmon";
  foo.olive = "olive";
  foo.cranberry = "cranberry";
  foo.boysenberry = "boysenberry";
  foo.blackberry = "blackberry";
  foo.passionfruit = "passionfruit";
  foo.mulberry = "mulberry";
  foo.marionberry = "marionberry";
  foo.plantain = "plantain";
  foo.lemon = "lemon";
  foo.yuzu = "yuzu";
  foo.loquat = "loquat";
  foo.kumquat = "kumquat";
  foo.salmonberry = "salmonberry";
  foo.tangerine = "tangerine";
  foo.durian = "durian";
  foo.pear = "pear";
  foo.cantaloupe = "cantaloupe";
  foo.quince = "quince";
  foo.guava = "guava";
  foo.strawberry = "strawberry";
  foo.nance = "nance";
  foo.apricot = "apricot";
  foo.jambul = "jambul";
  foo.grapefruit = "grapefruit";
  foo.clementine = "clementine";
  foo.jujube = "jujube";
  foo.cherry = "cherry";
  foo.feijoa = "feijoa";
  foo.jackfruit = "jackfruit";
  foo.fig = "fig";
  foo.cherimoya = "cherimoya";
  foo.pineapple = "pineapple";
  foo.blueberry = "blueberry";
  foo.jabuticaba = "jabuticaba";
  foo.miracle = "miracle";
  foo.dragonfruit = "dragonfruit";
  foo.satsuma = "satsuma";
  foo.tamarillo = "tamarillo";
  foo.honeydew = "honeydew";
}

setFields();

fun printFields() {
  print foo.apple; // expect: apple
  print foo.apricot; // expect: apricot
  print foo.avocado; // expect: avocado
  print foo.banana; // expect: banana
  print foo.bilberry; // expect: bilberry
  print foo.blackberry; // expect: blackberry
  print foo.blackcurrant; // expect: blackcurrant
  print foo.blueberry; // expect: blueberry
  print foo.boysenberry; // expect: boysenberry
  print foo.cantaloupe; // expect: cantaloupe
  print foo.cherimoya; // expect: cherimoya
  print foo.cherry; // expect: cherry
  print foo.clementine; // expect: clementine
  print foo.cloudberry; // expect: cloudberry
  print foo.coconut; // expect: coconut
  print foo.cranberry; // expect: cranberry
  print foo.currant; // expect: currant
  print foo.damson; // expect: damson
  print foo.date; // expect: date
  print foo.dragonfruit; // expect: dragonfruit
  print foo.durian; // expect: durian
  print foo.elderberry; // expect: elderberry
  print foo.feijoa; // expect: feijoa
  print foo.fig; // expect: fig
  print foo.gooseberry; // expect: gooseberry
  print foo.grape; // expect: grape
  print foo.grapefruit; // expect: grapefruit
  print foo.guava; // expect: guava
  print foo.honeydew; // expect: honeydew
  print foo.huckleberry; // expect: huckleberry
  print foo.jabuticaba; // expect: jabuticaba
  print foo.jackfruit; // expect: jackfruit
  print foo.jambul; // expect: jambul
  print foo.jujube; // expect: jujube
  print foo.juniper; // expect: juniper
  print foo.kiwifruit; // expect: kiwifruit
  print foo.kumquat; // expect: kumquat
  print foo.lemon; // expect: lemon
  print foo.lime; // expect: lime
  print foo.longan; // expect: longan
  print foo.loquat; // expect: loquat
  print foo.lychee; // expect: lychee
  print foo.mandarine; // expect: mandarine
  print foo.mango; // expect: mango
  print foo.marionberry; // expect: marionberry
  print foo.melon; // expect: melon
  print foo.miracle; // expect: miracle
  print foo.mulberry; // expect: mulberry
  print foo.nance; // expect: nance
  print foo.nectarine; // expect: nectarine
  print foo.olive; // expect: olive
  print foo.orange; // expect: orange
  print foo.papaya; // expect: papaya
  print foo.passionfruit; // expect: passionfruit
  print foo.peach; // expect: peach
  print foo.pear; // expect: pear
  print foo.persimmon; // expect: persimmon
  print foo.physalis; // expect: physalis
  print foo.pineapple; // expect: pineapple
  print foo.plantain; // expect: plantain
  print foo.plum; // expect: plum
  print foo.plumcot; // expect: plumcot
  print foo.pomegranate; // expect: pomegranate
  print foo.pomelo; // expect: pomelo
  print foo.quince; // expect: quince
  print foo.raisin; // expect: raisin
  print foo.rambutan; // expect: rambutan
  print foo.raspberry; // expect: raspberry
  print foo.redcurrant; // expect: redcurrant
  print foo.salak; // expect: salak
  print foo.salmonberry; // expect: salmonberry
  print foo.satsuma; // expect: satsuma
  print foo.strawberry; // expect: strawberry
  print foo.tamarillo; // expect: tamarillo
  print foo.tamarind; // expect: tamarind
  print foo.tangerine; // expect: tangerine
  print foo.tomato; // expect: tomato
  print foo.watermelon; // expect: watermelon
  print foo.yuzu; // expect: yuzu
}

printFields();
    "#;
    check(CODE);
}

#[test]
fn on_instance() {
    const CODE: &str = r#"class Foo {}

var foo = Foo();

print foo.bar = "bar value"; // expect: bar value
print foo.baz = "baz value"; // expect: baz value

print foo.bar; // expect: bar value
print foo.baz; // expect: baz value
    "#;
    check(CODE);
}

#[test]
fn get_and_set_method() {
    const CODE: &str = r#"// Bound methods have identity equality.
class Foo {
  method(a) {
    print "method";
    print a;
  }
  other(a) {
    print "other";
    print a;
  }
}

var foo = Foo();
var method = foo.method;

// Setting a property shadows the instance method.
foo.method = foo.other;
foo.method(1);
// expect: other
// expect: 1

// The old method handle still points to the original method.
method(2);
// expect: method
// expect: 2
    "#;
    check(CODE);
}

#[test]
fn set_evaluation_order() {
    const CODE: &str = "undefined1.bar // expect runtime error: Undefined variable 'undefined1'.
  = undefined2;
    ";
    check(CODE);
}

#[test]
fn set_on_function() {
    const CODE: &str = r#"fun foo() {}

foo.bar = "value"; // expect runtime error: Only instances have fields.
    "#;
    check(CODE);
}

#[test]
fn get_on_class() {
    const CODE: &str = "class Foo {}
Foo.bar; // expect runtime error: Only instances have properties.
    ";
    check(CODE);
}

#[test]
fn get_on_bool() {
    const CODE: &str = "true.foo; // expect runtime error: Only instances have properties.
    ";
    check(CODE);
}

#[test]
fn undefined() {
    const CODE: &str = "class Foo {}
var foo = Foo();

foo.bar; // expect runtime error: Undefined property 'bar'.
    ";
    check(CODE);
}

#[test]
fn get_on_function() {
    const CODE: &str = "fun foo() {}

foo.bar; // expect runtime error: Only instances have properties.
    ";
    check(CODE);
}

#[test]
fn method() {
    const CODE: &str = r#"class Foo {
  bar(arg) {
    print arg;
  }
}

var bar = Foo().bar;
print "got method"; // expect: got method
bar("arg");          // expect: arg
    "#;
    check(CODE);
}

#[test]
fn set_on_string() {
    const CODE: &str = r#""str".foo = "value"; // expect runtime error: Only instances have fields.
    "#;
    check(CODE);
}

#[test]
fn call_function_field() {
    const CODE: &str = r#"class Foo {}

fun bar(a, b) {
  print "bar";
  print a;
  print b;
}

var foo = Foo();
foo.bar = bar;

foo.bar(1, 2);
// expect: bar
// expect: 1
// expect: 2
    "#;
    check(CODE);
}

#[test]
fn set_on_nil() {
    const CODE: &str = r#"nil.foo = "value"; // expect runtime error: Only instances have fields.
    "#;
    check(CODE);
}
