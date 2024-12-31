use extendr_api::prelude::*;
use libm::{ceil, log10, sqrt};
use num::pow::pow;

/// Factors of a number new
/// @export
#[extendr]
fn factors(input:i64, include_original:bool) -> Vec<i64> {
    let mut poss: Vec<i64> = (1..=(sqrt(input as f64) as i64)).collect();
    poss.retain(|&x| input % x == 0);
    let mut poss2: Vec<i64> = poss.iter().map(|&x| input / x).collect();
    poss2.retain(|&x| x != (sqrt(input as f64) as i64));
    if !include_original {
        poss2.retain(|&x| x != input)
    }
    poss2.sort();
    poss.append(&mut poss2);
    return poss;
}

/// Digital sum of a number
/// @export
#[extendr]
fn digital_sum(n: i64) -> i64 {
    
    let mut temp = n;

    while temp >= 10 {
        let digits = ceil(log10((temp + 1) as f64)) as i64;
        let dig_v: Vec<i64> = (1..=digits).collect();
        temp = dig_v.iter().map(|&x| (temp % pow(10_i64, x as usize)) / pow(10_i64, (x - 1) as usize)).sum();
    }

    return temp;

}

/// Area of a polyon new
/// @export
#[extendr]
fn polygon_area(x: &[f64], y: &[f64]) -> f64 {

    let mut area: f64 = x.windows(2).zip(y.windows(2)).map(|(x, y)| {
       (x[1] + x[0]) * (y[1] - y[0]) / 2.0
    }).sum();

    area += (x[0] + x[x.len() - 1]) * (y[0] - y[y.len() - 1]) / 2.0;

    return area
}

/// Replace all text before a given character
/// @export
#[extendr]
fn replace_before(string: Vec<String>, symbol: &str, replacement: &str) -> extendr_api::Result<Vec<String>> {
    let results: Vec<String> = string
        .into_iter()    
        .map(|s| {
            if let Some(pos) = s.rfind(symbol) {
                format!("{}{}", replacement, &s[pos + symbol.len()..])
            } else {
                s
            }
        })
        .collect();

    Ok(results)
}

/// Replace all text after a given character
/// @export
#[extendr]
fn replace_after(string: Vec<String>, symbol: &str, replacement: &str) -> extendr_api::Result<Vec<String>> {
    let results: Vec<String> = string
        .into_iter()    
        .map(|s| {
            if let Some(pos) = s.find(symbol) {
                format!("{}{}", &s[..pos], replacement)
            } else {
                s
            }
        })
        .collect();

    Ok(results)

}


// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod sumo;
    fn factors;
    fn digital_sum;
    fn polygon_area;
    fn replace_after;
    fn replace_before;
}

