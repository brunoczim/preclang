#[macro_export]
macro_rules! assert_spanless_eq {
    ($left:expr, $right:expr $(,)?) => {
        $crate::assert_spanless_eq! { $left, $right, "" }
    };

    ($left:expr, $right:expr, $($fmt:tt)*) => {
        assert! {
            $crate::location::SpanlessEq::spanless_eq(&$left, &$right),
            "Found Spanless(left != right)\nLeft: {:#?}\nRight: {:#?}\n{}",
            $left,
            $right,
            format_args!($($fmt)*),
        }
    };
}

#[macro_export]
macro_rules! assert_spanless_ne {
    ($left:expr, $right:expr $(,)?) => {
        $crate::assert_spanless_ne! { $left, $right, "" }
    };

    ($left:expr, $right:expr, $($fmt:tt)*) => {
        assert! {
            !$crate::location::SpanlessEq::spanless_eq(&$left, &$right),
            "Found Spanless(left == right)\nLeft: {:#?}\nRight: {:#?}\n{}",
            $left,
            $right,
            format_args!($($fmt)*),
        }
    };
}
