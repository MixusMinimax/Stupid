use duplicate::duplicate_item;

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

#[duplicate_item(T; [i8]; [i16]; [i32]; [i64]; [u8]; [u16]; [u32]; [u64]; [bool])]
impl MyInto<bool> for T {
    fn into(self) -> bool {
        self != T::default()
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
