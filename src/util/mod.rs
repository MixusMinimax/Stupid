pub trait MyInto<T: Sized> {
    fn into(self) -> T;
}

impl<'a, T: Sized> MyInto<Option<&'a T>> for &'a Option<T> {
    fn into(self) -> Option<&'a T> {
        match self {
            Some(e) => Some(e),
            None => None,
        }
    }
}
