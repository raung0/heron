use std::collections::HashMap;

use crate::frontend::InitializerItem;
use crate::frontend::{AST, ASTValue, FrontendError, iterate_ast};

pub fn pass_1(ast: Box<AST>) -> Vec<FrontendError> {
    let mut errors = Vec::<FrontendError>::new();

    iterate_ast(&ast, None, &mut |node, _parent| match &node.v {
        ASTValue::Index { indices, .. } => {
            for idx in indices {
                if !is_id_or_dot_id_path(idx) {
                    errors.push(FrontendError::InvalidEnumeratedArrayEnum(
                        idx.location.clone(),
                    ));
                }
            }
        }

        ASTValue::InitializerList(items) => {
            check_keyed_initializer_duplicates(items, &mut errors);
        }

        ASTValue::TypedInitializerList { items, .. } => {
            check_keyed_initializer_duplicates(items, &mut errors);
        }

        _ => {}
    });

    errors
}

fn is_id_or_dot_id_path(n: &Box<AST>) -> bool {
    match &n.v {
        ASTValue::Id(_) => true,

        ASTValue::ExprList(parts) | ASTValue::ExprListNoScope(parts) => {
            if parts.is_empty() {
                return false;
            }

            if !matches!(&parts[0].v, ASTValue::Id(_)) {
                return false;
            }

            for p in parts.iter().skip(1) {
                if !matches!(&p.v, ASTValue::DotId(_)) {
                    return false;
                }
            }

            true
        }

        _ => false,
    }
}

fn check_keyed_initializer_duplicates(
    items: &Vec<InitializerItem>,
    errors: &mut Vec<FrontendError>,
) {
    let mut first: HashMap<&str, crate::frontend::SourceLocation> = HashMap::new();

    for item in items {
        let InitializerItem::Named { name, value } = item else {
            continue;
        };

        let loc = value.location.clone();

        if let Some(first_loc) = first.get(name.as_str()) {
            errors.push(FrontendError::InitializerListHasDuplicateFields {
                first_found_definition: first_loc.clone(),
                conflicting_definition: loc,
            });
        } else {
            first.insert(name.as_str(), loc);
        }
    }
}
