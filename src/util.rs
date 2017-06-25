extern crate libc;
use std::ffi::CString;

#[no_mangle]
pub unsafe extern fn destroy_string(string: *mut libc::c_char) {
    if !string.is_null() {
        CString::from_raw(string);
    }
}
