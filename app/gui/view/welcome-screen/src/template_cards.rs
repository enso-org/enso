//! Template cards for Welcome Screen.
//!
//! Template cards allow user to select a predefined template by clicking on the corresponding
//! card.

use ensogl::prelude::*;
use ensogl::system::web::traits::*;

use crate::ClickableElement;

use enso_frp as frp;
use ensogl::system::web;
use web::Element;
use web::HtmlDivElement;
use web::JsCast;



// ======================
// === Template Cards ===
// ======================


// === CardDefinition struct. ===

struct CardDefinition {
    class:                &'static str,
    background_image_url: Option<&'static str>,
    header:               &'static str,
    content:              &'static str,
    template:             &'static str,
}


// === Predefined cards. ===

const CARD_SPREADSHEETS: CardDefinition = CardDefinition {
    class:                crate::css_class::CARD_SPREADSHEETS,
    background_image_url: Some("/spreadsheets.png"),
    header:               "Combine spreadsheets",
    content:              "Glue multiple spreadsheets together to analyse all your data at once.",
    template:             "orders",
};
const CARD_GEO: CardDefinition = CardDefinition {
    class:                crate::css_class::CARD_GEO,
    background_image_url: None,
    header:               "Geospatial analysis",
    content:              "Learn where to open a coffee shop to maximize your income.",
    template:             "restaurants",
};
const CARD_VISUALIZE: CardDefinition = CardDefinition {
    class:                crate::css_class::CARD_VISUALIZE,
    background_image_url: None,
    header:               "Analyze GitHub stars",
    content:              "Find out which of Enso's repositories are most popular over time.",
    template:             "stargazers",
};


// === Card struct. ===

#[derive(Debug, Clone)]
struct Card {
    pub clickable_element: ClickableElement,
    pub template_name:     &'static str,
}

impl Deref for Card {
    type Target = ClickableElement;
    fn deref(&self) -> &Self::Target {
        &self.clickable_element
    }
}


// =============
// === Model ===
// =============

#[derive(Debug, Clone, CloneRef)]
pub struct Model {
    pub root_dom: Element,
    cards:        Rc<Vec<Card>>,
}

impl Model {
    /// Constructor.
    pub fn new(open_template: &frp::Any<String>) -> Self {
        let root_dom = web::document.create_element_or_panic("main");
        root_dom.set_class_name(crate::css_class::CONTENT);
        let templates = web::document.create_div_or_panic();

        let header = Self::create_header("Templates");
        templates.append_or_warn(&header);

        let (cards_dom, cards) = Self::create_cards();
        templates.append_or_warn(&cards_dom);
        root_dom.append_or_warn(&templates);

        let model = Self { root_dom, cards: Rc::new(cards) };
        model.setup_event_listeners(open_template);
        model
    }

    /// Attach click event for every card to `open_template` FRP endpoint.
    fn setup_event_listeners(&self, open_template: &frp::Any<String>) {
        for card in self.cards.iter() {
            let network = &card.network;
            let template_name = card.template_name.to_owned();
            frp::extend! { network
                open_template <+ card.click.constant(template_name);
            }
        }
    }

    fn create_header(content: &str) -> Element {
        let header = web::document.create_element_or_panic("h2");
        header.set_text_content(Some(content));
        header
    }

    /// Create main content, a set of cards.
    fn create_cards() -> (HtmlDivElement, Vec<Card>) {
        let mut cards = Vec::new();
        let dom = web::document.create_div_or_panic();
        dom.set_class_name(crate::css_class::CARDS);

        let row1 = Self::create_row(&[CARD_SPREADSHEETS, CARD_GEO], &mut cards);
        dom.append_or_warn(&row1);

        let row2 = Self::create_row(&[CARD_VISUALIZE], &mut cards);
        dom.append_or_warn(&row2);

        (dom, cards)
    }

    fn create_row(definitions: &[CardDefinition], cards: &mut Vec<Card>) -> HtmlDivElement {
        let row = web::document.create_div_or_panic();
        row.set_class_name(crate::css_class::ROW);
        for definition in definitions {
            let card = Self::create_card(definition);
            row.append_or_warn(&card.element);
            cards.push(card.clone());
        }
        row
    }

    /// Helper to create a single card DOM from provided definition.
    fn create_card(definition: &CardDefinition) -> Card {
        let card = web::document.create_div_or_panic();
        card.set_class_name(&format!("{} {}", crate::css_class::CARD, definition.class));
        if let Some(src) = definition.background_image_url {
            let img = web::document.create_element_or_panic("img");
            img.set_attribute_or_warn("src", src);
            card.append_or_warn(&img);
        }
        let card_header = web::document.create_element_or_panic("h3");
        card_header.set_text_content(Some(definition.header));
        card.append_or_warn(&card_header);
        let text_content = web::document.create_element_or_panic("p");
        text_content.set_text_content(Some(definition.content));
        card.append_or_warn(&text_content);

        let clickable_element = ClickableElement::new(card.unchecked_into());
        Card { clickable_element, template_name: definition.template }
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {}
    Output {
        // Create a new project from template `name`.
        open_template(String),
    }
}



// =====================
// === TemplateCards ===
// =====================

/// Template Cards for Welcome View. It contains a few predefined template cards. Clicking on
/// a template card creates a new project with some prepared code.
#[derive(Debug, Clone, CloneRef)]
pub struct TemplateCards {
    pub model: Model,
    pub frp:   Frp,
}

impl Deref for TemplateCards {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl TemplateCards {
    pub fn new() -> Self {
        let frp = Frp::new();
        let model = Model::new(&frp.output.source.open_template);
        Self { model, frp }
    }
}
