//! Struct specs: instance methods, `self`, and inheritance.

use poo::interpreter::Value;
use poo::run_source_checked;

#[test]
fn spec_struct_instance_method_can_access_self_properties() {
    let result = run_source_checked(
        r#"
        struct Person {
            name str

            impl {
                poof get_name() >> str {
                    return self.name;
                }
            }
        }

        poo person <: Person::{
            name: "Shayy"
        };

        return person.get_name();
        "#
        .to_string(),
    );

    assert_eq!(
        result.expect("expected successful execution"),
        Some(Value::String("Shayy".to_string()))
    );
}

#[test]
fn spec_struct_inheritance_exposes_parent_methods_on_child_instance() {
    let result = run_source_checked(
        r#"
        struct Person {
            name str

            impl {
                poof get_name() >> str {
                    return self.name;
                }
            }
        }

        struct Student inherits Person {
            student_id int
        }

        poo student <: Student::{
            name: "Ada",
            student_id: 7
        };

        return student.get_name();
        "#
        .to_string(),
    );

    assert_eq!(
        result.expect("expected successful execution"),
        Some(Value::String("Ada".to_string()))
    );
}
