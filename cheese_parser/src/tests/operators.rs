use crate::{args, v_map};
use crate::ast::{DeclarationFlags, Operator};
use crate::tests::{Error, v_empty_def, v_i, validate};
use std::collections::HashMap;
use crate::validation::{AstValidator, v_closure, v_const_reference_implicit_capture, v_none, v_single, v_variable_declaration, v_argument, v_bool, v_reference_capture, v_reference, v_constant_reference_capture, v_copy_capture, v_reference_implicit_capture, v_copy_implicit_capture, v_void, v_function, v_signed_integer_type, v_if, v_lesser_than, v_name, v_unary_minus, v_match, v_match_arm, v_match_value, v_match_range, v_match_constraint, v_match_all, v_destructuring_match_array, v_destructuring_match_structure, v_destructuring_match_arm, v_destructuring_match_tuple, v_enum_literal, v_match_enum_tuple, v_match_enum_structure, v_operator, v_self_value, v_block, v_self_type, v_unary_plus, v_dereference, v_not, v_multiplication, v_division, v_modulus, v_addition, v_subtraction, v_left_shift, v_right_shift, v_greater_than, v_lesser_equal, v_greater_equal, v_equal_to, v_not_equal_to, v_and, v_xor, v_or, v_reassign, v_assign, v_add_assign, v_subtract_assign, v_multiply_assign, v_divide_assign, v_modulate_assign, v_shift_left_assign, v_shift_right_assign, v_and_assign, v_or_assign, v_xor_assign};



// We won't have static operator overloading with this version of the language
fn v_operator_overload_unary(operator: Operator) -> Box<AstValidator> {
    v_single(
        v_operator(
            DeclarationFlags::empty(),
            operator,
            None,
            vec![v_self_value()],
            None,
            v_block(vec![])
        )
    )
}

fn v_operator_overload_binary(operator: Operator) -> Box<AstValidator> {
    v_single(
        v_operator(
            DeclarationFlags::empty(),
            operator,
            None,
            vec![v_self_value(), v_argument(None,v_self_type())],
            None,
            v_block(vec![])
        )
    )
}

fn v_operator_unary<T: Fn(Box<AstValidator>) -> Box<AstValidator>>(operator: T) -> Box<AstValidator>{
    v_single(
        v_variable_declaration(
            v_empty_def(),
            operator(v_name("y"))
        )
    )
}

fn v_operator_binary<T: Fn(Box<AstValidator>, Box<AstValidator>) -> Box<AstValidator>>(operator: T) -> Box<AstValidator>{
    v_single(
        v_variable_declaration(
            v_empty_def(),
            operator(v_name("y"), v_name("z"))
        )
    )
}

#[test]
fn tuple_call_overloading() -> Error {
    validate(
        "operator () (self) {}",
        v_operator_overload_unary(Operator::TupleCall)
    )
}

#[test]
fn array_call_overloading() -> Error {
    validate(
        "operator [] (self) {}",
        v_operator_overload_unary(Operator::ArrayCall)
    )
}

#[test]
fn object_call_overloading() -> Error {
    validate(
        "operator {} (self) {}",
        v_operator_overload_unary(Operator::ObjectCall)
    )
}

#[test]
fn subscript_overloading() -> Error {
    validate(
        "operator . (self) {}",
        v_operator_overload_unary(Operator::Subscript)
    )
}

#[test]
fn unary_plus() -> Error {
    validate(
        "let x = +y;",
        v_operator_unary(v_unary_plus)
    )
}

#[test]
fn unary_plus_overloading() -> Error {
    validate(
        "operator +? (self) {}",
        v_operator_overload_unary(Operator::UnaryPlus)
    )
}

#[test]
fn unary_minus() -> Error {
    validate(
        "let x = -y;",
        v_operator_unary(v_unary_minus)
    )
}

#[test]
fn unary_minus_overloading() -> Error {
    validate(
        "operator -? (self) {}",
        v_operator_overload_unary(Operator::UnaryMinus)
    )
}

#[test]
fn dereference() -> Error {
    validate(
        "let x = y$;",
        v_operator_unary(v_dereference)
    )
}

#[test]
fn dereference_overloading() -> Error {
    validate(
        "operator $ (self) {}",
        v_operator_overload_unary(Operator::Dereference)
    )
}

#[test]
fn not() -> Error {
    validate(
        "let x = not y;",
        v_operator_unary(v_not)
    )
}

#[test]
fn not_overloading() -> Error {
    validate(
        "operator not (self) {}",
        v_operator_overload_unary(Operator::Not)
    )
}

#[test]
fn multiply() -> Error {
    validate(
        "let x = y*z;",
        v_operator_binary(v_multiplication)
    )
}

#[test]
fn multiply_overloading() -> Error {
    validate(
        "operator * (self, _: Self) {}",
        v_operator_overload_binary(Operator::Multiply)
    )
}

#[test]
fn divide() -> Error {
    validate(
        "let x = y/z;",
        v_operator_binary(v_division)
    )
}

#[test]
fn divide_overloading() -> Error {
    validate(
        "operator / (self, _: Self) {}",
        v_operator_overload_binary(Operator::Divide)
    )
}

#[test]
fn modulus() -> Error {
    validate(
        "let x = y%z;",
        v_operator_binary(v_modulus)
    )
}

#[test]
fn modulus_overloading() -> Error {
    validate(
        "operator % (self, _: Self) {}",
        v_operator_overload_binary(Operator::Modulate)
    )
}

#[test]
fn add() -> Error {
    validate(
        "let x = y+z;",
        v_operator_binary(v_addition)
    )
}

#[test]
fn add_overloading() -> Error {
    validate(
        "operator + (self, _: Self) {}",
        v_operator_overload_binary(Operator::Add)
    )
}

#[test]
fn subtract() -> Error {
    validate(
        "let x = y-z;",
        v_operator_binary(v_subtraction)
    )
}

#[test]
fn subtract_overloading() -> Error {
    validate(
        "operator - (self, _: Self) {}",
        v_operator_overload_binary(Operator::Subtract)
    )
}

#[test]
fn left_shift() -> Error {
    validate(
        "let x = y<<z;",
        v_operator_binary(v_left_shift)
    )
}

#[test]
fn left_shift_overloading() -> Error {
    validate(
        "operator << (self, _: Self) {}",
        v_operator_overload_binary(Operator::ShiftLeft)
    )
}

#[test]
fn right_shift() -> Error {
    validate(
        "let x = y>>z;",
        v_operator_binary(v_right_shift)
    )
}

#[test]
fn right_shift_overloading() -> Error {
    validate(
        "operator >> (self, _: Self) {}",
        v_operator_overload_binary(Operator::ShiftRight)
    )
}

#[test]
fn less_than() -> Error {
    validate(
        "let x = y<z;",
        v_operator_binary(v_lesser_than)
    )
}

#[test]
fn less_than_overloading() -> Error {
    validate(
        "operator < (self, _: Self) {}",
        v_operator_overload_binary(Operator::Lesser)
    )
}

#[test]
fn greater_than() -> Error {
    validate(
        "let x = y>z;",
        v_operator_binary(v_greater_than)
    )
}

#[test]
fn greater_than_overloading() -> Error {
    validate(
        "operator > (self, _: Self) {}",
        v_operator_overload_binary(Operator::Greater)
    )
}

#[test]
fn less_than_equal() -> Error {
    validate(
        "let x = y<=z;",
        v_operator_binary(v_lesser_equal)
    )
}

#[test]
fn less_than_equal_overloading() -> Error {
    validate(
        "operator <= (self, _: Self) {}",
        v_operator_overload_binary(Operator::LesserEqual)
    )
}

#[test]
fn greater_than_equal() -> Error {
    validate(
        "let x = y>=z;",
        v_operator_binary(v_greater_equal)
    )
}

#[test]
fn greater_than_equal_overloading() -> Error {
    validate(
        "operator >= (self, _: Self) {}",
        v_operator_overload_binary(Operator::GreaterEqual)
    )
}

#[test]
fn equal_to() -> Error {
    validate(
        "let x = y==z;",
        v_operator_binary(v_equal_to)
    )
}

#[test]
fn equal_to_overloading() -> Error {
    validate(
        "operator == (self, _: Self) {}",
        v_operator_overload_binary(Operator::Equal)
    )
}

#[test]
fn not_equal_to() -> Error {
    validate(
        "let x = y!=z;",
        v_operator_binary(v_not_equal_to)
    )
}

#[test]
fn not_equal_to_overloading() -> Error {
    validate(
        "operator != (self, _: Self) {}",
        v_operator_overload_binary(Operator::NotEqual)
    )
}

#[test]
fn and() -> Error {
    validate(
        "let x = y and z;",
        v_operator_binary(v_and)
    )
}

#[test]
fn and_overloading() -> Error {
    validate(
        "operator and (self, _: Self) {}",
        v_operator_overload_binary(Operator::And)
    )
}

#[test]
fn or() -> Error {
    validate(
        "let x = y or z;",
        v_operator_binary(v_or)
    )
}

#[test]
fn or_overloading() -> Error {
    validate(
        "operator or (self, _: Self) {}",
        v_operator_overload_binary(Operator::Or)
    )
}

#[test]
fn xor() -> Error {
    validate(
        "let x = y xor z;",
        v_operator_binary(v_xor)
    )
}

#[test]
fn xor_overloading() -> Error {
    validate(
        "operator xor (self, _: Self) {}",
        v_operator_overload_binary(Operator::Xor)
    )
}

// #[test]
// fn reassign() -> Error {
//     validate(
//         "let x = y := z;",
//         v_operator_binary(v_reassign)
//     )
// }

#[test]
fn assign() -> Error {
    validate(
        "let x = y = z;",
        v_operator_binary(v_assign)
    )
}

#[test]
fn assign_overloading() -> Error {
    validate(
        "operator = (self, _: Self) {}",
        v_operator_overload_binary(Operator::Assign)
    )
}

#[test]
fn add_assign() -> Error {
    validate(
        "let x = y += z;",
        v_operator_binary(v_add_assign)
    )
}

#[test]
fn add_assign_overloading() -> Error {
    validate(
        "operator += (self, _: Self) {}",
        v_operator_overload_binary(Operator::AddAssign)
    )
}

#[test]
fn subtract_assign() -> Error {
    validate(
        "let x = y -= z;",
        v_operator_binary(v_subtract_assign)
    )
}

#[test]
fn subtract_assign_overloading() -> Error {
    validate(
        "operator -= (self, _: Self) {}",
        v_operator_overload_binary(Operator::SubtractAssign)
    )
}

#[test]
fn multiply_assign() -> Error {
    validate(
        "let x = y *= z;",
        v_operator_binary(v_multiply_assign)
    )
}

#[test]
fn multiply_assign_overloading() -> Error {
    validate(
        "operator *= (self, _: Self) {}",
        v_operator_overload_binary(Operator::MultiplyAssign)
    )
}

#[test]
fn divide_assign() -> Error {
    validate(
        "let x = y /= z;",
        v_operator_binary(v_divide_assign)
    )
}

#[test]
fn divide_assign_overloading() -> Error {
    validate(
        "operator /= (self, _: Self) {}",
        v_operator_overload_binary(Operator::DivideAssign)
    )
}

#[test]
fn modulus_assign() -> Error {
    validate(
        "let x = y %= z;",
        v_operator_binary(v_modulate_assign)
    )
}

#[test]
fn modulus_assign_overloading() -> Error {
    validate(
        "operator %= (self, _: Self) {}",
        v_operator_overload_binary(Operator::ModulateAssign)
    )
}

#[test]
fn left_shift_assign() -> Error {
    validate(
        "let x = y <<= z;",
        v_operator_binary(v_shift_left_assign)
    )
}

#[test]
fn left_shift_assign_overloading() -> Error {
    validate(
        "operator <<= (self, _: Self) {}",
        v_operator_overload_binary(Operator::ShiftLeftAssign)
    )
}
#[test]
fn right_shift_assign() -> Error {
    validate(
        "let x = y >>= z;",
        v_operator_binary(v_shift_right_assign)
    )
}

#[test]
fn right_shift_assign_overloading() -> Error {
    validate(
        "operator >>= (self, _: Self) {}",
        v_operator_overload_binary(Operator::ShiftRightAssign)
    )
}

#[test]
fn and_assign() -> Error {
    validate(
        "let x = y and= z;",
        v_operator_binary(v_and_assign)
    )
}

#[test]
fn and_assign_overloading() -> Error {
    validate(
        "operator and= (self, _: Self) {}",
        v_operator_overload_binary(Operator::AndAssign)
    )
}

#[test]
fn or_assign() -> Error {
    validate(
        "let x = y or= z;",
        v_operator_binary(v_or_assign)
    )
}

#[test]
fn or_assign_overloading() -> Error {
    validate(
        "operator or= (self, _: Self) {}",
        v_operator_overload_binary(Operator::OrAssign)
    )
}

#[test]
fn xor_assign() -> Error {
    validate(
        "let x = y xor= z;",
        v_operator_binary(v_xor_assign)
    )
}

#[test]
fn xor_assign_overloading() -> Error {
    validate(
        "operator xor= (self, _: Self) {}",
        v_operator_overload_binary(Operator::XorAssign)
    )
}