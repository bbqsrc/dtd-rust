use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while_m_n},
    character::is_space,
    combinator::all_consuming,
    combinator::{map_res, not},
    multi::separated_list,
    sequence::{delimited, tuple},
    IResult,
};

#[derive(Debug)]
struct Element {
    name: String,
    children: ElementChild,
}

#[derive(Debug)]
enum Optionality {
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
    Required,
}

#[derive(Debug)]
enum ElementChild {
    Child(String, Optionality),
    AnyOf(Vec<ElementChild>, Optionality),
    OneOf(Vec<ElementChild>, Optionality),
    Any,
    Empty
}

#[derive(Debug)]
enum AttListValue {
    IdRef,
    IdRefs,
    Entity,
    Entities,
    NmToken,
    NmTokens,
    Cdata,
    Values(Vec<String>)
}

#[derive(Debug)]
enum AttListDefault {
    None,
    Implied,
    Required,
    Fixed(String),
}

#[derive(Debug)]
struct AttList {
    element_name: String,
    name: String,
    value: AttListValue,
    default: AttListDefault,
}

#[derive(Debug)]
enum Tag {
    Element(Element),
    AttList(AttList),
    Comment(String),
}

fn parse_attlist_values(input: &str) -> IResult<&str, Vec<&str>> {
    delimited(tag("("), separated_list(tag("|"), take_while(|x| x != '|')), tag(")"))(input)
}

fn parse_attlist(input: &str) -> IResult<&str, AttList> {
    let (input, _) = take_while(|x| x == ' ')(input)?;
    let (input, element_name) = take_until(" ")(input)?;
    let (input, _) = take_while(|x| x == ' ')(input)?;
    let (input, name) = take_until(" ")(input)?;
    let (input, _) = take_while(|x| x == ' ')(input)?;
    
    let (input, value) = match parse_attlist_values(input) {
        Ok((input, list)) => {
            (input, AttListValue::Values(list.iter().map(|x| x.trim().to_string()).collect()))
        },
        Err(_) => {
            let (input, value) = take_until(" ")(input)?;
            (input, AttListValue::Cdata)
        }
    };

    let (input, _) = take_while(|x| x == ' ')(input)?;
    let (input, default) = take_until(">")(input)?;

    let default = match default.trim() {
        "#IMPLIED" => AttListDefault::Implied,
        "#REQUIRED" => AttListDefault::Required,
        x if x.starts_with("#FIXED") => {
            AttListDefault::Fixed(x.split(" ").skip(1).next().unwrap().to_string())
        },
        _ => AttListDefault::None,
    };

    Ok((input, AttList {
        element_name: element_name.to_string(),
        name: name.to_string(),
        value,
        default
    }))
}

// fn parse_element_child(input: &str) -> IResult<&str, ElementChild> {

// }

fn optionality(ch: char) -> Optionality {
    if ch == '?' {
        Optionality::ZeroOrOne
    } else if ch == '*' {
        Optionality::ZeroOrMore
    } else if ch == '+' {
        Optionality::OneOrMore
    } else {
        Optionality::Required
    }
}

fn parse_op_suffix(input: &str) -> IResult<&str, &str> {
    let (op, input) = take_while(|x| x != '?' && x != '*' && x != '+')(input)?;
    Ok((input, op))
}

fn parse_element_child(input: &str) -> IResult<&str, ElementChild> {
    let mut input = input.trim();
    if input == "" {
        panic!("impossible");
    }

    let (input, op) = parse_op_suffix(input)?;
    let op = optionality(op.chars().next().unwrap_or('\0'));
    return Ok(("", ElementChild::Child(input.to_string(), op)));
}

fn any_tag(input: &str) -> IResult<&str, &str> {
    tag("ANY")(input)
}

fn empty_tag(input: &str) -> IResult<&str, &str> {
    tag("EMPTY")(input)
}

fn parse_element_children(mut input: &str) -> IResult<&str, ElementChild> {
    match any_tag(input) {
        Ok((input, _)) => {
            return Ok((input, ElementChild::Any));
        },
        Err(_) => {}
    }

    match empty_tag(input) {
        Ok((input, _)) => {
            return Ok((input, ElementChild::Empty));
        },
        Err(_) => {}
    }

    input = tag("(")(input)?.0;

    let mut out = vec![];
    let mut is_disjoint = false;
    loop {
        input = take_while(|x| x == ' ')(input)?.0;
        let (inner_input, child) = take_while(|x| x != ',' && x != '|' && x != ')')(input)?;
        
        if child.trim() != "" {
            let (_, child) = parse_element_child(child)?;
            out.push(child);
        }
        
        if inner_input.starts_with(")") {
            let (inner_input, _) = tag(")")(inner_input)?;
            let optionality = match inner_input.chars().next() {
                Some(ch) => optionality(ch),
                None => Optionality::Required
            };

            let child = if is_disjoint {
                ElementChild::OneOf(out, optionality)
            } else {
                ElementChild::AnyOf(out, optionality)
            };
            return Ok((inner_input, child))
        } else if inner_input.starts_with("|") {
            is_disjoint = true;
            let (inner_input, _) = tag("|")(inner_input)?;
        }

        let (inner_input, child_candidate) = take_while(|x| x == '|' || x == ',' || x == ' ')(inner_input)?;
        if inner_input.starts_with("(") {
            let (inner_input, child) = parse_element_children(inner_input)?;
            out.push(child);
            input = inner_input;
        } else {
            input = inner_input;
        }
    }
}

fn parse_element(input: &str) -> IResult<&str, Element> {
    let (input, _) = take_while(|x| x == ' ')(input)?;
    let (input, name) = take_until(" ")(input)?;
    let (input, _) = take_while(|x| x == ' ')(input)?;
    // Ok((input, Element { name: name.to_string() }));
    let (input, children) = parse_element_children(input)?;
    // unimplemented!()
    Ok((
        input,
        Element {
            name: name.to_string(),
            children,
        },
    ))
}

fn start_tag(input: &str) -> IResult<&str, &str> {
    take_until("<!")(input)
}

fn parse_tag(input: &str) -> IResult<&str, Option<Tag>> {
    // println!("OK");
    let input = match start_tag(input) {
        Ok((v, _)) => v,
        Err(_) => return Ok(("", None))
    };

    // println!("Took: {}", input);
    let (input, _) = tag("<!")(input)?; //, take_until(">"), tag(">"))(input)?;
    let (input, _) = take_while(|x| x == ' ')(input)?;

    let (input, tag_name) = alt((tag("ELEMENT"), tag("ATTLIST"), tag("--")))(input)?;
    match tag_name {
        "ELEMENT" => {
            let (input, body) = take_until(">")(input)?;
            let (input, _) = tag(">")(input)?;
            Ok((input, Some(Tag::Element(parse_element(body)?.1))))
        },
        "ATTLIST" => {
            Ok((input, Some(Tag::AttList(parse_attlist(input)?.1))))
        }
        "--" => {
            let (input, comment) = take_until("-->")(input)?;
            let (input, _) = tag("-->")(input)?;
            Ok((input, Some(Tag::Comment(comment.to_string()))))
        }
        _ => panic!(),
    }
}

fn parse_dtd(mut input: &str) {
    loop {
        match parse_tag(input) {
            Ok((next_input, Some(tag))) => {
                println!("{:?}", tag);
                input = next_input;
            },
            Ok((_, None)) => {
                break;
            }
            Err(e) => {
                eprintln!("{:?}", e);
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        parse_dtd(include_str!("../ldml.dtd"))
    }
}
