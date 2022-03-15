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

impl<'a, T: Sized> MyInto<Option<&'a mut T>> for &'a mut Option<T> {
    fn into(self) -> Option<&'a mut T> {
        match self {
            Some(e) => Some(e),
            None => None,
        }
    }
}


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
