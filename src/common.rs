use anyhow::{anyhow, Context, Result};
use std::{
    convert::TryFrom,
    fmt::{Debug, Display},
};
pub fn lift_soln<F, I, O, E>(f: F) -> Box<dyn Fn(String) -> Result<String>>
where
    F: Fn(I) -> O + 'static,
    I: TryFrom<String, Error = E>,
    O: Display,
    E: Debug,
{
    Box::new(move |text| {
        let input = I::try_from(text);
        match input {
            Ok(i) => Ok(format!("result: {}", f(i))),
            Err(e) => Err(anyhow!("failed to parse input: {:?}", e)),
        }
    })
}
