use anyhow::{bail, Error};
use handlebars::Handlebars;
use rust_embed::RustEmbed;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

pub type Templates = Handlebars<'static>;

#[derive(RustEmbed)]
#[folder = "templates"]
struct TemplateBlobs;

pub fn create_templates() -> Result<Templates, Error> {
    let mut h = Handlebars::new();
    h.set_strict_mode(true);
    h.register_embed_templates::<TemplateBlobs>()?;

    // assert none is missing
    let missing = Template::iter()
        .map(|x| x.name())
        .filter_map(|x| if h.has_template(x) { None } else { Some(x) })
        .collect::<Vec<_>>();
    if !missing.is_empty() {
        let names = missing.join(", ");
        bail!("missing templates: {}", names)
    }

    Ok(h)
}

#[derive(EnumIter, Debug, Clone, Copy)]
pub enum Template {
    HomePageHtml,
    PrefixesPageHtml,
    OpensearchXml,
    RobotsTxt,
}

impl Template {
    pub const fn name(&self) -> &'static str {
        use Template::*;
        match self {
            HomePageHtml => "home_page.html.hbs",
            PrefixesPageHtml => "prefixes_page.html.hbs",
            OpensearchXml => "opensearch.xml.hbs",
            RobotsTxt => "robots.txt.hbs",
        }
    }
}
