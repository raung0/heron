use std::collections::HashMap;

use crate::frontend::{AST, ASTValue, FrontendError, walk_ast};
use crate::frontend::{InitializerItem, Type};

pub(crate) fn pass_1(ast: Box<AST>) -> Vec<FrontendError> {
    let mut errors = Vec::<FrontendError>::new();

    walk_ast(&ast, None, &mut |node, _parent| match &node.v {
        ASTValue::TypedInitializerList { ty, items } => {
            check_keyed_initializer_duplicates(items, &mut errors);
            if !is_id_or_dot_id_path(ty) {
                errors.push(FrontendError::InvalidEnumeratedArrayEnum(
                    node.location.clone(),
                ));
            }
        }

        ASTValue::InitializerList(items) => {
            check_keyed_initializer_duplicates(items, &mut errors);
        }

        _ => {}
    });

    errors
}

fn is_id_or_dot_id_path(n: &Box<Type>) -> bool {
    match &**n {
        Type::Id(_) => true,
        // FIXME: Add parsing for .
        _ => false,
    }
}

fn check_keyed_initializer_duplicates(items: &[InitializerItem], errors: &mut Vec<FrontendError>) {
    let mut first: HashMap<String, crate::frontend::SourceLocation> = HashMap::new();

    for item in items {
        let InitializerItem::Named { name, value } = item else {
            continue;
        };

        let loc = value.location.clone();

        if let Some(first_loc) = first.insert(name.clone(), loc.clone()) {
            errors.push(FrontendError::InitializerListHasDuplicateFields {
                first_found_definition: first_loc,
                conflicting_definition: loc,
            });
        }
    }
}
