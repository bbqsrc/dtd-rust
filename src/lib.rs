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
    AllOf(Vec<ElementChild>, Optionality),
    OneOf(Vec<ElementChild>, Optionality),
    Any,
    Empty,
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
    Values(Vec<String>),
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
    delimited(
        tag("("),
        separated_list(tag("|"), take_while(|x| x != '|')),
        tag(")"),
    )(input)
}

fn parse_attlist(input: &str) -> IResult<&str, AttList> {
    let (input, _) = take_while(|x| x == ' ')(input)?;
    let (input, element_name) = take_until(" ")(input)?;
    let (input, _) = take_while(|x| x == ' ')(input)?;
    let (input, name) = take_until(" ")(input)?;
    let (input, _) = take_while(|x| x == ' ')(input)?;

    let (input, value) = match parse_attlist_values(input) {
        Ok((input, list)) => (
            input,
            AttListValue::Values(list.iter().map(|x| x.trim().to_string()).collect()),
        ),
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
        }
        _ => AttListDefault::None,
    };

    Ok((
        input,
        AttList {
            element_name: element_name.to_string(),
            name: name.to_string(),
            value,
            default,
        },
    ))
}

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
        }
        Err(_) => {}
    }

    match empty_tag(input) {
        Ok((input, _)) => {
            return Ok((input, ElementChild::Empty));
        }
        Err(_) => {}
    }

    input = tag("(")(input)?.0;

    let mut out = vec![];
    let mut is_disjoint = false;
    loop {
        input = take_while(|x| x == ' ')(input)?.0;
        let (mut inner_input, child) = take_while(|x| x != ',' && x != '|' && x != ')')(input)?;

        if child.trim() != "" {
            let (_, child) = parse_element_child(child)?;
            out.push(child);
        }

        let inner_input = if inner_input.starts_with(")") {
            let (inner_input, _) = tag(")")(inner_input)?;
            let optionality = match inner_input.chars().next() {
                Some(ch) => optionality(ch),
                None => Optionality::Required,
            };
            let inner_input = take_while(|x| x == '?' || x == '*' || x == '+')(inner_input)?.0;

            let child = if is_disjoint {
                ElementChild::OneOf(out, optionality)
            } else {
                ElementChild::AllOf(out, optionality)
            };
            return Ok((inner_input, child));
        } else if inner_input.starts_with("|") {
            is_disjoint = true;
            tag("|")(inner_input)?.0
        } else {
            inner_input
        };

        let (inner_input, _) = take_while(|x| x == '|' || x == ',' || x == ' ')(inner_input)?;
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
    let (input, children) = parse_element_children(input)?;
    let (input, _) = take_until(">")(input)?;
    let (input, _) = tag(">")(input)?;

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
    let input = match start_tag(input) {
        Ok((v, _)) => v,
        Err(_) => return Ok(("", None)),
    };

    let (input, _) = tag("<!")(input)?;
    let (input, _) = take_while(|x| x == ' ')(input)?;

    let (input, tag_name) = alt((tag("ELEMENT"), tag("ATTLIST"), tag("--")))(input)?;
    match tag_name {
        "ELEMENT" => Ok((input, Some(Tag::Element(parse_element(input)?.1)))),
        "ATTLIST" => Ok((input, Some(Tag::AttList(parse_attlist(input)?.1)))),
        "--" => {
            let (input, comment) = take_until("-->")(input)?;
            let (input, _) = tag("-->")(input)?;
            Ok((input, Some(Tag::Comment(comment.to_string()))))
        }
        _ => panic!(),
    }
}

fn parse_dtd(mut input: &str) -> Result<Vec<Tag>, ()> {
    let mut out = vec![];
    loop {
        match parse_tag(input) {
            Ok((next_input, Some(tag))) => {
                out.push(tag);
                input = next_input;
            }
            Ok((_, None)) => {
                return Ok(out);
            }
            Err(e) => {
                return Err(());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        parse_dtd(include_str!("../ldml.dtd"));
    }
}

mod gen {
    use super::{parse_dtd, AttListDefault, Element, ElementChild, Optionality, Tag};

    #[derive(Debug)]
    struct XmlField {
        name: String,
        ty: XmlTy,
        optionality: Optionality,
    }

    #[derive(Debug)]
    struct XmlElement {
        name: String,
        ty: XmlTy,
        fields: Vec<XmlField>,
    }

    #[derive(Debug, PartialEq)]
    enum XmlTy {
        Struct,
        AnonStruct,
        Enum,
        AnonEnum,
        Attr,
    }

    use std::collections::HashMap;

    #[derive(Debug)]
    struct Gen {
        elements: HashMap<String, XmlElement>,
    }

    impl Gen {
        fn process(&mut self, name: &str, child: ElementChild, depth: usize) -> Vec<XmlField> {
            let mut out = vec![];

            match child {
                ElementChild::Empty => {}
                ElementChild::Any => {
                    eprintln!("WARNING: ANY found, dislike downvote");
                }
                ElementChild::Child(name, optionality) => {
                    out.push(XmlField {
                        name,
                        ty: XmlTy::Struct,
                        optionality,
                    });
                }
                ElementChild::AllOf(children, optionality) => {
                    if depth == 0 {
                        for child in children {
                            out.append(&mut self.process(name, child, depth + 1));
                        }
                        return out;
                    }

                    let mut variant = 1;
                    let mut variant_name = format!("{}_AllOf{}", name, variant);

                    while self.elements.contains_key(&variant_name) {
                        variant += 1;
                        variant_name = format!("{}_AllOf{}", name, variant);
                    }

                    self.elements.insert(
                        variant_name.clone(),
                        XmlElement {
                            name: variant_name.clone(),
                            ty: XmlTy::AnonStruct,
                            fields: vec![],
                        },
                    );

                    let mut fields = vec![];
                    for child in children {
                        fields.append(&mut self.process(&variant_name, child, depth + 1));
                    }
                    self.elements.get_mut(&variant_name).unwrap().fields = fields;

                    out.push(XmlField {
                        name: variant_name.clone(),
                        optionality,
                        ty: XmlTy::AnonStruct,
                    });
                }
                ElementChild::OneOf(children, optionality) => {
                    if depth == 0 {
                        for child in children {
                            out.append(&mut self.process(name, child, depth + 1));
                        }
                        return out;
                    }

                    let mut variant = 1;
                    let mut variant_name = format!("{}_OneOf{}", name, variant);

                    while self.elements.contains_key(&variant_name) {
                        variant += 1;
                        variant_name = format!("{}_OneOf{}", name, variant);
                    }

                    self.elements.insert(
                        variant_name.clone(),
                        XmlElement {
                            name: variant_name.clone(),
                            ty: XmlTy::AnonEnum,
                            fields: vec![],
                        },
                    );

                    let mut fields = vec![];
                    for child in children {
                        fields.append(&mut self.process(&variant_name, child, depth + 1));
                    }
                    self.elements.get_mut(&variant_name).unwrap().fields = fields;

                    out.push(XmlField {
                        name: variant_name.clone(),
                        optionality,
                        ty: XmlTy::AnonEnum,
                    });
                }
            }

            out
        }

        fn new() -> Gen {
            Gen {
                elements: HashMap::new(),
            }
        }

        fn print(&self) {
            use heck::CamelCase;
            use heck::SnakeCase;
            let mut keys = self.elements.keys().collect::<Vec<_>>();
            keys.sort();
            for key in keys {
                let element = &self.elements[key];
                match element.ty {
                    XmlTy::Struct | XmlTy::AnonStruct => {
                        println!("#[derive(Debug, Serialize, Deserialize)]");
                        println!("struct {} {{", &element.name.to_camel_case());
                        for field in element.fields.iter() {
                            let ty = if field.name == "#PCDATA" {
                                "String".into()
                            } else {
                                match (&field.ty, &field.optionality) {
                                    (XmlTy::Attr, Optionality::Required) => "String".into(),
                                    (XmlTy::Attr, _) => "Option<String>".into(),
                                    (_, Optionality::Required) => {
                                        field.name.to_string().to_camel_case()
                                    }
                                    (_, Optionality::ZeroOrMore) => {
                                        format!("Vec<{}>", &field.name.to_camel_case())
                                    }
                                    (_, Optionality::OneOrMore) => {
                                        format!("Vec<{}>", &field.name.to_camel_case())
                                    }
                                    (_, Optionality::ZeroOrOne) => {
                                        format!("Option<{}>", &field.name.to_camel_case())
                                    }
                                }
                            };

                            let name = if field.name == "#PCDATA" {
                                println!("    #[serde(rename = \"$value\")]");
                                "_pcdata".into()
                            } else {
                                match field.ty {
                                    XmlTy::AnonStruct => {
                                        println!("    // Generated anonymous struct")
                                    }
                                    XmlTy::AnonEnum => println!("    // Generated anonymous enum"),
                                    _ => println!("    #[serde(rename = \"{}\")]", field.name),
                                }
                                field.name.to_snake_case()
                            };
                            println!("    {}: {},", name, ty);
                        }
                        println!("}}\n");
                    }
                    XmlTy::Enum | XmlTy::AnonEnum => {
                        println!("#[derive(Debug, Serialize, Deserialize)]");
                        println!("#[serde(untagged)]");
                        println!("enum {} {{", &element.name.to_camel_case());
                        for field in element.fields.iter() {
                            let ty = if field.name == "#PCDATA" {
                                "String".into()
                            } else {
                                match (&field.ty, &field.optionality) {
                                    (XmlTy::Attr, Optionality::Required) => "String".into(),
                                    (XmlTy::Attr, _) => "Option<String>".into(),
                                    (_, Optionality::Required) => {
                                        field.name.to_string().to_camel_case()
                                    }
                                    (_, Optionality::ZeroOrMore) => {
                                        format!("Vec<{}>", &field.name.to_camel_case())
                                    }
                                    (_, Optionality::OneOrMore) => {
                                        format!("Vec<{}>", &field.name.to_camel_case())
                                    }
                                    (_, Optionality::ZeroOrOne) => {
                                        format!("Option<{}>", &field.name.to_camel_case())
                                    }
                                }
                            };
                            let name = if field.name == "#PCDATA" {
                                println!("    #[serde(rename = \"$value\")]");
                                "_pcdata".into()
                            } else {
                                match field.ty {
                                    XmlTy::AnonStruct => {
                                        println!("    // Generated anonymous struct")
                                    }
                                    XmlTy::AnonEnum => println!("    // Generated anonymous enum"),
                                    _ => println!("    #[serde(rename = \"{}\")]", field.name),
                                }
                                field.name.to_camel_case()
                            };
                            println!("    {}({}),", &name, &ty);
                        }
                        println!("}}\n");
                    }
                    _ => {}
                }
            }
        }

        fn generate(&mut self, tags: Vec<Tag>) {
            self.elements = HashMap::new();

            for tag in tags {
                match tag {
                    Tag::AttList(attlist) => {
                        let element = match self.elements.get_mut(&attlist.element_name) {
                            Some(v) => v,
                            None => panic!("No element: {}", &attlist.element_name),
                        };
                        element.fields.push(XmlField {
                            name: attlist.name,
                            ty: XmlTy::Attr,
                            optionality: match attlist.default {
                                AttListDefault::Required => Optionality::Required,
                                _ => Optionality::ZeroOrMore,
                            },
                        });
                    }
                    Tag::Element(element) => match element.children {
                        ElementChild::Empty | ElementChild::Any => {
                            self.elements.insert(
                                element.name.clone(),
                                XmlElement {
                                    name: element.name.clone(),
                                    ty: XmlTy::Struct,
                                    fields: vec![],
                                },
                            );
                        }
                        ElementChild::AllOf(_, _) => {
                            let fields = self.process(&element.name, element.children, 0);
                            self.elements.insert(
                                element.name.clone(),
                                XmlElement {
                                    name: element.name.clone(),
                                    ty: XmlTy::Struct,
                                    fields,
                                },
                            );
                        }
                        ElementChild::OneOf(_, _) => {
                            let fields = self.process(&element.name, element.children, 0);
                            self.elements.insert(
                                element.name.clone(),
                                XmlElement {
                                    name: element.name.clone(),
                                    ty: XmlTy::Enum,
                                    fields,
                                },
                            );
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
        }
    }

    #[cfg(test)]
    #[test]
    fn test() {
        let mut g = Gen::new();
        let tags = parse_dtd(include_str!("../ldml.dtd")).unwrap();
        println!("{:#?}", tags);
        g.generate(tags);
        g.print();
    }
}
