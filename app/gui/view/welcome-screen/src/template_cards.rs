//! Template cards for Welcome Screen.
//!
//! Template cards allow user to select a predefined template by clicking on the corresponding
//! card.

use ensogl::prelude::*;

use crate::ClickClosure;

use ensogl::system::web;
use ensogl::system::web::AttributeSetter;
use ensogl::system::web::NodeInserter;
use web_sys::Element;
use web_sys::HtmlDivElement;



// ========================
// === Cards Definition ===
// ========================

struct Card<'a> {
    id:        &'a str,
    class:     &'a str,
    image_url: Option<&'a str>,
    header:    &'a str,
    content:   &'a str,
}

const CARD_SPREADSHEETS: Card<'static> = Card {
    id:        "card-spreadsheets",
    class:     "card card-spreadsheets",
    image_url: Some("/assets/spreadsheets.png"),
    header:    "Combine spreadsheets",
    content:   "Glue multiple spreadsheets together to analyse all your data at once.",
};
const CARD_GEO: Card<'static> = Card {
    id:        "card-geo",
    class:     "card card-geo",
    image_url: None,
    header:    "Geospatial analysis",
    content:   "Learn where to open a coffee shop to maximize your income.",
};
const CARD_VISUALIZE: Card<'static> = Card {
    id:        "card-visualize",
    class:     "card card-visualize",
    image_url: None,
    header:    "Analyze GitHub stars",
    content:   "Find out which of Enso's repositories are most popular over time.",
};



// ======================
// === Template Cards ===
// ======================

/// Template Cards for Welcome View. It contains a few predefined template cards. Clicking on
/// a template card creates a new project with some prepared code.
#[derive(Debug, Clone, CloneRef)]
pub struct TemplateCards {
    logger:       Logger,
    pub root_dom: Element,
    closures:     Rc<RefCell<Vec<ClickClosure>>>,
}

impl TemplateCards {
    /// Constructor.
    pub fn new(logger: &Logger) -> Self {
        let logger = Logger::new_sub(logger, "TemplateCards");
        let root_dom = web::create_element("main");
        root_dom.set_class_name("content");

        let templates = {
            let templates = web::create_div();
            let header = {
                let header = web::create_element("h2");
                header.set_text_content(Some("Templates"));
                header
            };
            templates.append_or_warn(&header, &logger);
            let cards = Self::create_cards(&logger);
            templates.append_or_warn(&cards, &logger);

            templates
        };
        root_dom.append_or_warn(&templates, &logger);

        Self { logger, root_dom, closures: default() }
    }

    /// Create main content, a set of cards.
    fn create_cards(logger: &Logger) -> HtmlDivElement {
        let cards = web::create_div();
        cards.set_class_name("cards");

        let row1 = {
            let row = web::create_div();
            row.set_class_name("row");

            let card_spreadsheets = Self::create_card(logger, CARD_SPREADSHEETS);
            row.append_or_warn(&card_spreadsheets, logger);

            let card_geo = Self::create_card(logger, CARD_GEO);
            row.append_or_warn(&card_geo, logger);

            row
        };
        cards.append_or_warn(&row1, logger);

        let row2 = {
            let row = web::create_div();
            row.set_class_name("row");

            let card_visualize = Self::create_card(logger, CARD_VISUALIZE);
            row.append_or_warn(&card_visualize, logger);

            row
        };
        cards.append_or_warn(&row2, logger);

        cards
    }

    /// Helper to create a single card with provided HTML attributes, optional image, header and
    /// content text.
    fn create_card(logger: &Logger, definition: Card<'_>) -> HtmlDivElement {
        let card = web::create_div();
        card.set_id(definition.id);
        card.set_class_name(definition.class);
        if let Some(src) = definition.image_url {
            let img = web::create_element("img");
            img.set_attribute_or_warn("src", src, logger);
            card.append_or_warn(&img, logger);
        }
        let card_header = web::create_element("h3");
        card_header.set_text_content(Some(definition.header));
        card.append_or_warn(&card_header, logger);
        let text_content = web::create_element("p");
        text_content.set_text_content(Some(definition.content));
        card.append_or_warn(&text_content, logger);

        card
    }
}
