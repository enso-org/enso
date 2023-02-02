#![feature(extend_one)]
#![feature(let_chains)]
#![feature(if_let_guard)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use enso_prelude::*;

use ast::prelude::FallibleResult;

mod translation;


// ==============
// === Export ===
// ==============

pub use ast::IdMap;
pub use parser_scala::DocParser;

pub mod api;


#[derive(Debug, Clone, CloneRef)]
pub struct Parser {
    parser: Rc<enso_parser::Parser>,
}


// === Core methods provided by the underlying parser ===

impl Parser {
    pub fn new() -> Self {
        let parser = Rc::new(enso_parser::Parser::new());
        Self { parser }
    }

    pub fn parse(&self, program: String, ids: IdMap) -> api::Result<ast::Ast> {
        let tree = self.parser.run(&program);
        Ok(translation::to_legacy_ast(&tree))
    }

    pub fn parse_with_metadata<M: api::Metadata>(
        &self,
        program: String,
    ) -> api::Result<api::ParsedSourceFile<M>> {
        let (code, meta) = enso_parser::metadata::extract(&program);
        let tree = self.parser.run(code);
        // TODO: Log errors.
        let metadata = meta.and_then(|meta| serde_json::from_str(meta).ok()).unwrap_or_default();
        let ast = ast::known::Module::try_from(translation::to_legacy_ast_module(&tree).unwrap())
            .unwrap();
        Ok(api::ParsedSourceFile { ast, metadata })
    }

    pub fn parse_module(&self, program: impl Str, ids: IdMap) -> api::Result<ast::known::Module> {
        let tree = self.parser.run(program.as_ref());
        let ast = translation::to_legacy_ast_module(&tree).unwrap();
        ast::known::Module::try_from(ast).map_err(|_| api::Error::NonModuleRoot)
    }
}


// === Convenience methods ===

impl Parser {
    pub fn parse_line_ast(&self, program: impl Str) -> FallibleResult<ast::Ast> {
        self.parse_line(program).map(|line| line.elem)
    }

    pub fn parse_line(&self, program: impl Str) -> FallibleResult<ast::BlockLine<ast::Ast>> {
        self.parse_line_with_id_map(program, default())
    }

    pub fn parse_line_ast_with_id_map(
        &self,
        program: impl Str,
        id_map: IdMap,
    ) -> FallibleResult<ast::Ast> {
        self.parse_line_with_id_map(program, id_map).map(|line| line.elem)
    }

    /// Program is expected to be single non-empty line module. Return the parsed line.
    fn parse_line_with_id_map(
        &self,
        program: impl Str,
        id_map: IdMap,
    ) -> FallibleResult<ast::BlockLine<ast::Ast>> {
        let module = self.parse_module(program, id_map)?;
        let mut lines =
            module.lines.clone().into_iter().filter_map(|line| line.map(|elem| elem).transpose());
        if let Some(first_non_empty_line) = lines.next() {
            if lines.next().is_some() {
                Err(api::TooManyLinesProduced.into())
            } else {
                Ok(first_non_empty_line)
            }
        } else {
            Err(api::NoLinesProduced.into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::HasRepr;

    #[test]
    fn test_group_repr() {
        let code = "bar (Foo (a b))";
        let ast = Parser::new().parse_line_ast(code).unwrap();
        assert_eq!(ast.repr(), code);
    }

    #[test]
    fn test_text_repr() {
        let code = "operator17 = operator16.order_by (Sort_Column.Name 'Orders Value' Sort_Direction.Descending)";
        let ast = Parser::new().parse_line_ast(code).unwrap();
        assert_eq!(ast.repr(), code);
    }

    #[test]
    fn test_whole_file_repr() {
        let code = r#"
import Standard.Visualization
from Standard.Base import all
from Standard.Table import all
import Standard.Visualization
import Standard.Examples

main =
    operator2 = enso_project.data / 'store_data.xlsx'
    operator3 = operator2.read (Excel (Worksheet 'Customers'))
    operator4 = operator2.read (Excel (Worksheet 'Items'))
    operator5 = operator2.read (Excel (Worksheet 'Orders'))
    operator8 = operator5.join operator4  Join_Kind.Inner ['Item ID']
    operator1 = operator8.at 'Unit Price'
    operator9 = operator8.at 'Quantity'
    product1 = operator1 * operator9
    operator10 = operator8.set 'Order Value' product1
    operator11 = operator10.aggregate [Aggregate_Column.Group_By 'Customer ID', Aggregate_Column.Sum 'Order Value' 'Orders Value']
    operator16 = operator3.join operator11 Join_Kind.Inner ["Customer ID"]
    operator17 = operator16.order_by (Sort_Column.Name 'Orders Value' Sort_Direction.Descending)
"#;
        let ast = Parser::new().parse_module(code, default()).unwrap();
        assert_eq!(ast.repr(), code);
    }

    /*
        #[test]
        fn test_orders_repr() {
            let code = r#"
    from Standard.Base import all
    from Standard.Table import all
    import Standard.Visualization
    import Standard.Examples

    main =
        ## The file contains three different sheets relating to operations of an
           online store.
        operator2 = enso_project.data / 'store_data.xlsx'
        ## Read the customers table.
        operator3 = operator2.read (Excel (Worksheet 'Customers'))
        ## Read the products table.
        operator4 = operator2.read (Excel (Worksheet 'Items'))
        ## Read the orders history.
        operator5 = operator2.read (Excel (Worksheet 'Orders'))
        ## Join the item data to the order history, to get information on item
           prices in the orders table.
        operator8 = operator5.join operator4  Join_Kind.Inner ['Item ID']
        operator1 = operator8.at 'Unit Price'
        operator9 = operator8.at 'Quantity'
        ## Multiply item prices and counts to get total order value.
        product1 = operator1 * operator9
        operator10 = operator8.set 'Order Value' product1
        ## Group all orders by the Customer ID, to compute the total value of orders
           placed by each client.
        operator11 = operator10.aggregate [Aggregate_Column.Group_By 'Customer ID', Aggregate_Column.Sum 'Order Value' 'Orders Value']
        ## Join the customer data into orders table, to include names in the final
           ranking.
        operator16 = operator3.join operator11 Join_Kind.Inner ["Customer ID"]
        ## Sort the customers by their lifetime value, with the most valuable
           customers at the start of the table.
        operator17 = operator16.order_by (Sort_Column.Name 'Orders Value' Sort_Direction.Descending)



    #### METADATA ####
    [[{"index":{"value":5},"size":{"value":8}},"a4fb3b7a-1c68-49ea-9f91-99764d7c8f2f"],[{"index":{"value":13},"size":{"value":1}},"aa9e3aa5-8338-4f1f-886e-ffe40f25147b"],[{"index":{"value":14},"size":{"value":4}},"89a6b699-2dc5-46bb-a0fc-054f298c76ec"],[{"index":{"value":26},"size":{"value":3}},"0162a09f-5b45-49a3-97ff-00059eddc08c"],[{"index":{"value":0},"size":{"value":29}},"ce3ba3e1-6d6f-4436-9e70-70da0be83603"],[{"index":{"value":35},"size":{"value":8}},"8387f695-24b2-4d5d-8e01-39a62f80e137"],[{"index":{"value":43},"size":{"value":1}},"ca9b8ff1-5a75-46bf-bafb-2dbf5be8b26d"],[{"index":{"value":44},"size":{"value":5}},"15893706-cd86-43e9-9a33-8eb72b1ce469"],[{"index":{"value":57},"size":{"value":3}},"81cd9286-18c7-4174-aa0f-34ee944d2872"],[{"index":{"value":30},"size":{"value":30}},"75ec5bc5-9b10-4cd2-b779-8b60de9ed041"],[{"index":{"value":68},"size":{"value":8}},"1496a347-9d84-4eeb-a8ce-5c2270bc2c70"],[{"index":{"value":76},"size":{"value":1}},"1e47da49-5228-4679-a015-d315c9263bc1"],[{"index":{"value":77},"size":{"value":13}},"7f1b0af8-e321-4be4-bde9-016c3fb0994d"],[{"index":{"value":61},"size":{"value":29}},"dca79d23-c819-4c74-9f16-24a13208968c"],[{"index":{"value":98},"size":{"value":8}},"2f67bdad-570a-46b9-84a3-e7dab2abe6d7"],[{"index":{"value":106},"size":{"value":1}},"4eaea67a-37f8-4bc9-82d2-7e7dfe452d96"],[{"index":{"value":107},"size":{"value":8}},"ea1f4426-36e7-49d3-a9b5-1587d0e65c04"],[{"index":{"value":91},"size":{"value":24}},"43e4133b-a1c6-4bbd-b8ab-bdd5519a3e95"],[{"index":{"value":117},"size":{"value":4}},"bae19e34-9a19-4457-85a3-d68490c1af1c"],[{"index":{"value":122},"size":{"value":1}},"d8f4493f-8e6d-412e-9632-2f9560049ddc"],[{"index":{"value":131},"size":{"value":3}},"f229eb76-4bb9-4d82-8417-9c21c0c04cf5"],[{"index":{"value":135},"size":{"value":4}},"4710c2ae-a69d-4099-a91b-f18b7605abea"],[{"index":{"value":140},"size":{"value":8}},"f28e7156-fe9d-486c-9df5-a4879dff4aa6"],[{"index":{"value":149},"size":{"value":5}},"28d18841-37d0-4791-a097-c4f71c97bc02"],[{"index":{"value":155},"size":{"value":9}},"6177e5cb-4929-46d8-a23b-abc54d8547fd"],[{"index":{"value":165},"size":{"value":6}},"22259a61-31b3-4977-85b1-dd0229168d17"],[{"index":{"value":172},"size":{"value":8}},"14071145-9e4c-4089-a0dc-68516c1762de"],[{"index":{"value":181},"size":{"value":2}},"dc468192-3ee5-41b9-8cc4-6b86eb88d1c8"],[{"index":{"value":184},"size":{"value":10}},"5978eb7e-4c1c-453a-a2b2-9b898d403443"],[{"index":{"value":195},"size":{"value":2}},"4d8725df-2f54-4efa-a8cc-f780d32ed50a"],[{"index":{"value":198},"size":{"value":2}},"d0dbbff0-dec0-4716-943a-0a2df1549eba"],[{"index":{"value":208},"size":{"value":6}},"af64be81-b7fd-4205-aab6-6086a6aad3b2"],[{"index":{"value":215},"size":{"value":5}},"942800fd-257e-4b99-a6b2-c9c2d029d923"],[{"index":{"value":215},"size":{"value":6}},"bea5733f-9fd5-4647-98c6-80aaaf3d3ff7"],[{"index":{"value":208},"size":{"value":13}},"3d94625a-b057-453e-943f-04df50d3ece4"],[{"index":{"value":200},"size":{"value":21}},"e8e6bb54-4048-458f-bbec-5d934b75b0a4"],[{"index":{"value":128},"size":{"value":93}},"36012152-6aae-41a4-9748-56a9e6d8d0aa"],[{"index":{"value":226},"size":{"value":9}},"b1a5d42a-81cd-4f1c-9100-22fe058a3de6"],[{"index":{"value":236},"size":{"value":1}},"39d8469a-2985-4e79-ac0a-abef1c8a1757"],[{"index":{"value":238},"size":{"value":12}},"1d4fbdb5-7c7a-4826-a40f-d061040272f0"],[{"index":{"value":250},"size":{"value":1}},"1bafa0d9-0d1b-4511-86e6-0011efcee29f"],[{"index":{"value":251},"size":{"value":4}},"ae160851-95ce-40f5-84b3-d1573e61682f"],[{"index":{"value":238},"size":{"value":17}},"d96cad58-623c-4601-ba76-77b092463613"],[{"index":{"value":256},"size":{"value":1}},"a57763e5-b6ac-4132-9902-6ffe6a290846"],[{"index":{"value":258},"size":{"value":17}},"5192211d-6a84-4f7e-b935-9e2c116ee17c"],[{"index":{"value":238},"size":{"value":37}},"c0236d2f-2e37-439d-8aaa-a1bc7fa68f7d"],[{"index":{"value":226},"size":{"value":49}},"0a206b1c-578c-4ae6-8bc4-dec6f937d037"],[{"index":{"value":283},"size":{"value":4}},"98eab466-6036-42b9-a2b0-bf7890b44e3d"],[{"index":{"value":288},"size":{"value":3}},"f4cac5e1-40ad-46be-aeb2-19d53cfeae3b"],[{"index":{"value":292},"size":{"value":9}},"d8ba6d5b-f081-4f91-86d8-889b3f0f7193"],[{"index":{"value":302},"size":{"value":5}},"ff5dee49-3994-479b-b5d7-c8f1b7360911"],[{"index":{"value":307},"size":{"value":1}},"6b9edfed-9463-48bf-a9b0-f145042905eb"],[{"index":{"value":280},"size":{"value":28}},"1d83df06-6f94-4c61-9b43-97c6a11dd5e6"],[{"index":{"value":313},"size":{"value":9}},"6ccc8529-5336-49eb-89ff-8d41fd03aa0a"],[{"index":{"value":323},"size":{"value":1}},"7ab00d0d-cbb2-4742-9317-e46e95f9eea1"],[{"index":{"value":325},"size":{"value":9}},"d41b820d-0b8c-4704-b986-4447990b6335"],[{"index":{"value":334},"size":{"value":1}},"6982a2e4-7cf0-4ca7-9b3e-e8c5e1906169"],[{"index":{"value":335},"size":{"value":4}},"040e08be-0feb-4796-ae10-cff5f4ce203a"],[{"index":{"value":325},"size":{"value":14}},"15e8443b-a5c6-4f8e-913d-4e3f7dcca614"],[{"index":{"value":341},"size":{"value":5}},"e335210e-5aa2-458a-86c4-d1eb9d11783d"],[{"index":{"value":348},"size":{"value":9}},"f240f0f1-ae6d-4d1b-bed7-726a854d8da1"],[{"index":{"value":358},"size":{"value":11}},"70f09cf3-a951-4cab-9991-a16f369a064d"],[{"index":{"value":348},"size":{"value":21}},"160b564c-1eaa-43fd-92b1-65415472d2d7"],[{"index":{"value":347},"size":{"value":23}},"e2bb198f-db02-43b3-abf9-ce19a59c8db9"],[{"index":{"value":341},"size":{"value":29}},"761198fa-28e5-49a4-be41-15f712706e51"],[{"index":{"value":340},"size":{"value":31}},"744f55c6-7eea-4022-80a2-af8b886b68e5"],[{"index":{"value":325},"size":{"value":46}},"b804a267-ef59-47e5-a457-81b3395d7214"],[{"index":{"value":313},"size":{"value":58}},"bdb86b56-1f6e-436d-b118-94c4862fe969"],[{"index":{"value":379},"size":{"value":4}},"bff535d7-4174-448a-9c7c-176d2d562f12"],[{"index":{"value":384},"size":{"value":3}},"f719ccae-1482-4ba9-a8a3-b50f608e17ca"],[{"index":{"value":388},"size":{"value":8}},"37ce7f30-3b42-489b-a847-6e43a4ba0b7c"],[{"index":{"value":397},"size":{"value":5}},"9da40776-24b5-41a6-92b2-1e764ee462cb"],[{"index":{"value":402},"size":{"value":1}},"5066307c-cc68-460f-b329-4f91cac895e4"],[{"index":{"value":376},"size":{"value":27}},"19f18708-8148-4a8d-b49b-124e582c8640"],[{"index":{"value":408},"size":{"value":9}},"efae01b4-f996-4f0a-8308-543dc6c2fd96"],[{"index":{"value":418},"size":{"value":1}},"0a9a6b8c-4f6a-4849-b2c9-2698da4c22f3"],[{"index":{"value":420},"size":{"value":9}},"0251db52-04b9-4f6d-a7ec-35c453c8f565"],[{"index":{"value":429},"size":{"value":1}},"79d7f683-c5d4-4056-9160-2cf3c20c1b53"],[{"index":{"value":430},"size":{"value":4}},"edea943b-47a2-4296-bbe0-5ab96239e4c5"],[{"index":{"value":420},"size":{"value":14}},"1dcbf7cc-451e-4e1d-ae27-8ad932e3f4d5"],[{"index":{"value":436},"size":{"value":5}},"e95725ff-523d-4d0d-a1bd-a93eb035046c"],[{"index":{"value":443},"size":{"value":9}},"c30d8ac7-9fc9-45d8-b742-c6b26fa303a0"],[{"index":{"value":453},"size":{"value":7}},"da6898b1-a840-4f57-8c0e-4c58a1263ad5"],[{"index":{"value":443},"size":{"value":17}},"b60aadfd-7c0e-43fe-8fc6-7c023bed02cd"],[{"index":{"value":442},"size":{"value":19}},"c5c116b8-c7ef-4998-94e9-3f749926d676"],[{"index":{"value":436},"size":{"value":25}},"744cffd9-5f14-4882-981d-3a6001ec1944"],[{"index":{"value":435},"size":{"value":27}},"36ee93e6-23f3-4c4b-8e43-fc9d28602ea2"],[{"index":{"value":420},"size":{"value":42}},"ab238dcf-c234-46b3-b20b-de9daf667abf"],[{"index":{"value":408},"size":{"value":54}},"d4cfe26a-e727-4246-b162-35c02463fe54"],[{"index":{"value":470},"size":{"value":4}},"175b1be0-8dc0-4a84-801a-7f5995a1a01a"],[{"index":{"value":475},"size":{"value":3}},"b50a9fea-7a06-48dd-9b0c-d718664288b1"],[{"index":{"value":479},"size":{"value":6}},"000f01cc-1f51-4703-8f04-397d01ee24f9"],[{"index":{"value":486},"size":{"value":7}},"9dc0902a-1a18-4930-966c-cac01720679e"],[{"index":{"value":493},"size":{"value":1}},"00a1e1db-7e5a-4358-8974-570927ccd13e"],[{"index":{"value":467},"size":{"value":27}},"0d5a3f0c-1782-4731-b7c2-3784eb691eaa"],[{"index":{"value":499},"size":{"value":9}},"85ebe9f6-9af5-41ab-bf0c-b30aece24dd3"],[{"index":{"value":509},"size":{"value":1}},"e4475e96-f50c-45af-8e4c-bf37d8a5137d"],[{"index":{"value":511},"size":{"value":9}},"a2378c47-ec2d-4b91-9524-c933b0f5d338"],[{"index":{"value":520},"size":{"value":1}},"ebb7b6da-6f8d-4c97-b3d7-91d4b82a1913"],[{"index":{"value":521},"size":{"value":4}},"3ac59e2d-0bbb-4866-ad00-208e63548862"],[{"index":{"value":511},"size":{"value":14}},"381d8010-2b6e-4921-b56a-60fb216d4418"],[{"index":{"value":527},"size":{"value":5}},"43bdeaed-8372-477c-937f-70d01ef06fd0"],[{"index":{"value":534},"size":{"value":9}},"2947e776-be5d-4c00-9616-5671555b0a8a"],[{"index":{"value":544},"size":{"value":8}},"0ef240d3-103c-4b9f-a2e2-eecddb0dc066"],[{"index":{"value":534},"size":{"value":18}},"8850d52a-fd58-446a-828a-d9a08cb4fb43"],[{"index":{"value":533},"size":{"value":20}},"f22e1bc9-d30b-4d85-928d-df845ce96e48"],[{"index":{"value":527},"size":{"value":26}},"54416c8a-2a4f-4068-b9f7-cbbb6430a6e4"],[{"index":{"value":526},"size":{"value":28}},"59524fcb-b046-4bf9-bb69-f03f7a02f2e7"],[{"index":{"value":511},"size":{"value":43}},"da0e81da-f85b-4686-bd5c-2a3d3b02794f"],[{"index":{"value":499},"size":{"value":55}},"5e8abcf1-a326-4de6-b429-3b1268bad63c"],[{"index":{"value":562},"size":{"value":4}},"1e667779-506e-4f13-b740-b3bc37373e6d"],[{"index":{"value":567},"size":{"value":3}},"0b9531c8-f878-4499-ab57-b1f0b4759b35"],[{"index":{"value":571},"size":{"value":4}},"718f52fe-23b1-487c-b14e-fc11ce72c6eb"],[{"index":{"value":576},"size":{"value":4}},"9d97af3f-f979-49b2-97a2-8a0f7c83eff6"],[{"index":{"value":581},"size":{"value":2}},"b2dbe142-0908-4c1d-b8f3-6786f5e599bf"],[{"index":{"value":584},"size":{"value":3}},"175b3b0f-10c0-43fc-85a8-1c1f9d01e73c"],[{"index":{"value":588},"size":{"value":5}},"e0457f09-4471-44a3-9b80-b63652387aca"],[{"index":{"value":594},"size":{"value":7}},"d39da52f-0f8a-466e-968e-5bbd23a3ff3f"],[{"index":{"value":601},"size":{"value":1}},"6eed309e-5bff-47f3-b398-855f2286ae3f"],[{"index":{"value":603},"size":{"value":2}},"f498d959-c88a-4e76-8ff8-47e8f7c867ce"],[{"index":{"value":606},"size":{"value":3}},"032f8e4d-64cd-49a1-a094-4fe364862f27"],[{"index":{"value":610},"size":{"value":11}},"2819138e-e68f-4e74-972e-73840b5c20cb"],[{"index":{"value":622},"size":{"value":2}},"3999c0e6-742c-4d7a-b4dd-fec8d81035dc"],[{"index":{"value":625},"size":{"value":4}},"cf752491-24c1-483d-b8a0-1ddaded568aa"],[{"index":{"value":637},"size":{"value":6}},"7cc275b4-f1d6-4ad7-a045-2782f9be3fc1"],[{"index":{"value":644},"size":{"value":2}},"61a100dd-0680-4cc4-b576-ccc839839ecd"],[{"index":{"value":647},"size":{"value":3}},"5d9c4ae0-a0df-416e-8fbf-c58e7253005d"],[{"index":{"value":651},"size":{"value":6}},"f562aea8-9a5a-4ec2-b730-d9c25de8f872"],[{"index":{"value":647},"size":{"value":10}},"d8929e02-afb0-46f1-83c5-6a640d13855f"],[{"index":{"value":658},"size":{"value":5}},"2e3d1ee7-e060-40d1-8a21-d79ea55ad8e1"],[{"index":{"value":658},"size":{"value":6}},"6a3db75f-7c6f-4f13-a487-03a04f6d7af0"],[{"index":{"value":647},"size":{"value":17}},"37a1dc9e-3523-4374-9527-8e97709e00cd"],[{"index":{"value":637},"size":{"value":27}},"bf9ac667-0dcc-4798-983e-db16879d7af6"],[{"index":{"value":629},"size":{"value":35}},"b5d18e41-f9a6-49fb-939a-09b92fc40e8e"],[{"index":{"value":559},"size":{"value":105}},"b09447a7-c17c-4a40-885c-f5cbbccd62c3"],[{"index":{"value":669},"size":{"value":9}},"691a15fb-95b5-45a6-9883-5454c1036f89"],[{"index":{"value":679},"size":{"value":1}},"0bd07b15-81e7-48c9-8ec3-45fc7316e650"],[{"index":{"value":681},"size":{"value":9}},"21ee29aa-4382-4375-a349-eaca84b58aba"],[{"index":{"value":690},"size":{"value":1}},"8f4a6695-38a3-4290-bc19-f5ed9e7835d5"],[{"index":{"value":691},"size":{"value":4}},"89facf03-af9c-4e12-ac6f-8e0365dd5972"],[{"index":{"value":681},"size":{"value":14}},"160e3399-3452-44cb-8e6a-8bbbe693cebe"],[{"index":{"value":696},"size":{"value":9}},"c54de2e5-8f76-4930-81ae-170f0ab81a06"],[{"index":{"value":681},"size":{"value":24}},"f166b2fe-46f1-4030-befb-895da26dd8bf"],[{"index":{"value":707},"size":{"value":9}},"5fafaf09-dcbb-4065-bd45-a558dfc1311d"],[{"index":{"value":716},"size":{"value":1}},"2c385b3a-4144-440e-8686-108ac56d02ec"],[{"index":{"value":717},"size":{"value":5}},"e97b7487-2731-409e-97b4-5ba2426f82b8"],[{"index":{"value":707},"size":{"value":15}},"fd3d41b7-2336-4759-b83c-601ba8bacb35"],[{"index":{"value":681},"size":{"value":41}},"beec19f4-ae91-465b-8a4b-46672d56ea48"],[{"index":{"value":724},"size":{"value":9}},"bc995c67-a9b9-457e-90b9-69ebda4e4ecb"],[{"index":{"value":723},"size":{"value":11}},"0b5c4443-d8ae-40ea-8210-79ac38343e8e"],[{"index":{"value":681},"size":{"value":53}},"f6b10179-e443-4ede-8d85-de95027f84b8"],[{"index":{"value":669},"size":{"value":65}},"a11b79d3-1314-4df5-973a-d2f1735a5a63"],[{"index":{"value":739},"size":{"value":9}},"bd1daf7e-42c2-40d5-9d57-4537527242ca"],[{"index":{"value":749},"size":{"value":1}},"933239ac-fcbf-4494-97a4-c2db1e640456"],[{"index":{"value":751},"size":{"value":9}},"6d148d88-67dc-43d6-a086-4de2ab86aa9a"],[{"index":{"value":760},"size":{"value":1}},"2495fbbb-6d63-4c9e-9428-e53e47823130"],[{"index":{"value":761},"size":{"value":2}},"8e94a5ee-cd90-4b69-9d95-ac380667db7b"],[{"index":{"value":751},"size":{"value":12}},"72b1fc5f-dae1-4ac3-b9a9-e418abc5169b"],[{"index":{"value":764},"size":{"value":12}},"00bf5f1e-141a-478a-bb70-8ebfc032b56f"],[{"index":{"value":751},"size":{"value":25}},"6265c951-1d95-4e64-8520-b211ab326814"],[{"index":{"value":739},"size":{"value":37}},"f7edb24a-4c68-445d-994e-78b9dd3d89de"],[{"index":{"value":781},"size":{"value":9}},"b9fcb068-7621-4322-9686-39323ad9bc01"],[{"index":{"value":791},"size":{"value":1}},"c7ce0a4a-6f15-4f23-94eb-44abfee24da6"],[{"index":{"value":793},"size":{"value":9}},"2b3f0bcd-6518-4cc7-bb07-96fafc7f1e75"],[{"index":{"value":802},"size":{"value":1}},"6197330e-c02c-438a-b485-f6bbc7e10611"],[{"index":{"value":803},"size":{"value":2}},"98ca16bd-71da-438d-a9ca-00af697e9ad1"],[{"index":{"value":793},"size":{"value":12}},"0ec8d72a-df11-4489-8cf0-855c26e58d8f"],[{"index":{"value":806},"size":{"value":10}},"10b1a015-2d11-4a25-b05a-2ecc1a7fcbad"],[{"index":{"value":793},"size":{"value":23}},"64fa287f-e0e3-43b6-8ab2-2dd1925e2b67"],[{"index":{"value":781},"size":{"value":35}},"9856be93-11f0-4e4b-b269-f688f77275e8"],[{"index":{"value":824},"size":{"value":8}},"36e6e484-475a-45e6-95b1-212c02eaa15b"],[{"index":{"value":833},"size":{"value":4}},"82a55a9b-735e-482b-b062-301927176d18"],[{"index":{"value":838},"size":{"value":6}},"45e1ccbb-1075-411d-9aca-1835a7b74f46"],[{"index":{"value":845},"size":{"value":3}},"321501e1-da6c-48d0-a141-c7361e8bff9a"],[{"index":{"value":849},"size":{"value":6}},"4de71ef7-919b-4023-8153-14dfae6311aa"],[{"index":{"value":856},"size":{"value":2}},"6f0cad9e-a6c8-4a34-a124-315e5d31cd7c"],[{"index":{"value":859},"size":{"value":3}},"9d26cb2d-8e84-4916-90c1-15f56abd7d59"],[{"index":{"value":863},"size":{"value":5}},"c4c8c3f8-1ec2-41c1-b8f8-dbdd731a44e7"],[{"index":{"value":869},"size":{"value":5}},"fb259ed4-60c0-4b4a-867f-1d660e57d576"],[{"index":{"value":875},"size":{"value":5}},"ba0c5cda-7d6a-4d53-985f-c5628c549399"],[{"index":{"value":880},"size":{"value":1}},"8901f16b-2ded-41c4-b0d5-f0c40a2501f9"],[{"index":{"value":821},"size":{"value":60}},"f31efa41-c93e-4171-a3d2-6915c0fc9778"],[{"index":{"value":886},"size":{"value":8}},"18720c1b-7d89-40b6-aa97-dc320586ffc4"],[{"index":{"value":895},"size":{"value":1}},"ec9ff57c-0d31-46b4-9af7-ac6a940004ad"],[{"index":{"value":897},"size":{"value":9}},"60ef1a6a-b397-41ee-9896-0d9e798fbf6c"],[{"index":{"value":907},"size":{"value":1}},"f10a458c-8135-4b8f-bc8e-a2d3afb26b84"],[{"index":{"value":909},"size":{"value":9}},"15d57a1b-fb8d-42aa-8659-7fafac6cf592"],[{"index":{"value":897},"size":{"value":21}},"1213fb7c-dbb6-47a0-984b-0a4cbaa8cacd"],[{"index":{"value":886},"size":{"value":32}},"68ab8f3b-31e5-413e-a56a-a42980869de6"],[{"index":{"value":923},"size":{"value":10}},"021dd227-52a6-45db-8cbd-c1ac57b430e3"],[{"index":{"value":934},"size":{"value":1}},"05ac9099-8aac-4b11-af38-4a36dd140cb2"],[{"index":{"value":936},"size":{"value":9}},"c1699634-e0a2-413a-940f-b79911f8d252"],[{"index":{"value":945},"size":{"value":1}},"76aa01a8-94c0-4a34-901d-834fafbc0ee8"],[{"index":{"value":946},"size":{"value":3}},"d8350624-f40d-4f09-b940-3fb558fa8b82"],[{"index":{"value":936},"size":{"value":13}},"0edec44b-f8ad-486b-a450-e8546124e0b9"],[{"index":{"value":950},"size":{"value":13}},"e80c26ed-7c3e-4844-bc4c-abd7b38f3299"],[{"index":{"value":936},"size":{"value":27}},"6eb62ff1-6879-4892-8e0b-d298e4a09bf9"],[{"index":{"value":964},"size":{"value":8}},"b9829171-e6a3-4225-b180-3dcc0519998d"],[{"index":{"value":936},"size":{"value":36}},"63ae4e2c-f5ab-4828-a24a-da462d49d3f7"],[{"index":{"value":923},"size":{"value":49}},"00e67e92-658c-458b-a30f-3e31157f6cb1"],[{"index":{"value":980},"size":{"value":5}},"57b43675-accd-4dba-b57c-c68327068565"],[{"index":{"value":986},"size":{"value":3}},"bff160f3-00a0-45bf-a9db-69c31eb1d62c"],[{"index":{"value":990},"size":{"value":6}},"9e03b9ba-8fcb-413c-9bbb-6a37a752b2ae"],[{"index":{"value":997},"size":{"value":2}},"16099e71-df97-412b-b5cb-56c017afa730"],[{"index":{"value":1000},"size":{"value":3}},"9fde0ee0-c7ce-4e10-8105-565a9765fef0"],[{"index":{"value":1004},"size":{"value":8}},"01a64a6e-0a3c-4129-926b-eb384ccb3279"],[{"index":{"value":1013},"size":{"value":2}},"ef5d41e4-61f4-4ce3-a72d-442c13466738"],[{"index":{"value":1015},"size":{"value":1}},"0bfde177-b481-4890-975f-c04c8709d44e"],[{"index":{"value":1017},"size":{"value":2}},"43797704-fe65-4357-8590-88d2793d20a6"],[{"index":{"value":1020},"size":{"value":7}},"63b7b89d-f79e-426b-a187-6544d7270e4a"],[{"index":{"value":1028},"size":{"value":3}},"c2b22f43-d3f5-4e80-b531-2dd4f8f606f2"],[{"index":{"value":1032},"size":{"value":5}},"e89509f5-303f-4479-a5d4-80083eac5dde"],[{"index":{"value":1038},"size":{"value":5}},"2aeb9ce9-694d-4d65-9fca-999d04ae5ae5"],[{"index":{"value":1044},"size":{"value":2}},"f97f1118-ae28-41ac-b230-7da9cabd62cc"],[{"index":{"value":1047},"size":{"value":6}},"22f20800-fc21-4b85-80a2-c7fff90cd0db"],[{"index":{"value":1061},"size":{"value":6}},"29fbea7f-31de-4429-ba58-efd61582daca"],[{"index":{"value":1068},"size":{"value":2}},"98a04f5a-d47a-4b69-8ecf-dc2f69e3c0ac"],[{"index":{"value":1061},"size":{"value":9}},"e08e2e83-3ff4-40a8-b13b-89bea027bd30"],[{"index":{"value":1071},"size":{"value":4}},"5646a09c-882d-4afb-bc63-ebc1109464d8"],[{"index":{"value":1061},"size":{"value":14}},"c8ce51bb-8461-4a4f-a0d2-cc672adace6d"],[{"index":{"value":1076},"size":{"value":6}},"6af092be-6d5f-493b-b7b1-186a6eaee97f"],[{"index":{"value":1076},"size":{"value":7}},"c2a97176-c0b8-41df-a3b4-d06237b64d6c"],[{"index":{"value":1061},"size":{"value":22}},"5e5542d8-5653-4b4f-9c1a-12ac897cef48"],[{"index":{"value":1053},"size":{"value":30}},"668c37a5-6c61-4caa-9558-d2d6e4d255ad"],[{"index":{"value":977},"size":{"value":106}},"ab2014c7-69ce-4712-8417-4c2c9306f2b3"],[{"index":{"value":1088},"size":{"value":10}},"275ecbfe-b135-43b1-bbca-c22f97f36c35"],[{"index":{"value":1099},"size":{"value":1}},"e5d674f2-3b14-418d-ad52-eb7b241af785"],[{"index":{"value":1101},"size":{"value":10}},"668fcc1f-13d4-42fd-8d35-0bac140de8c2"],[{"index":{"value":1111},"size":{"value":1}},"4db569f5-23af-46ce-b2f0-e665bca6ca5d"],[{"index":{"value":1112},"size":{"value":9}},"99336e03-a07c-4475-8afe-e8c2d02c483a"],[{"index":{"value":1101},"size":{"value":20}},"76aeb01f-1f36-4ef6-a345-77770ace452b"],[{"index":{"value":1123},"size":{"value":16}},"df60b129-49c2-451a-85e1-295ad15a4f74"],[{"index":{"value":1139},"size":{"value":1}},"71d289f2-c01c-45b6-9189-b87c28a8e8b5"],[{"index":{"value":1140},"size":{"value":8}},"4f6c34da-143f-4ab9-b56f-ba952d29f82a"],[{"index":{"value":1123},"size":{"value":25}},"082e6cda-b1cc-4b40-af4a-d78e5af77a03"],[{"index":{"value":1149},"size":{"value":13}},"731c86c8-a5b4-4b56-b2f3-1ac5ae87e473"],[{"index":{"value":1123},"size":{"value":39}},"129e06df-ee09-459e-9590-238ee5ec34af"],[{"index":{"value":1162},"size":{"value":1}},"f9115d37-d654-4b4a-987b-4c99b8c02330"],[{"index":{"value":1164},"size":{"value":16}},"7c5d4b7a-df92-47c4-a762-3898f810ac30"],[{"index":{"value":1180},"size":{"value":1}},"cea62ac1-b56f-4f3d-835d-708967dd8b04"],[{"index":{"value":1181},"size":{"value":3}},"3f99048e-66e7-4c9a-8476-40dda519edd2"],[{"index":{"value":1164},"size":{"value":20}},"b74720d9-a390-4563-9fc4-a8321f632b5c"],[{"index":{"value":1185},"size":{"value":13}},"24cd958a-4a5a-4d68-8979-250fe70dc72b"],[{"index":{"value":1164},"size":{"value":34}},"a34b9bc1-70d7-4a5e-be8d-ef7cd0af5c4e"],[{"index":{"value":1199},"size":{"value":14}},"2642cd6b-72c6-4cef-871d-0320b4c80248"],[{"index":{"value":1164},"size":{"value":49}},"3472dc16-0a7c-4593-81e8-8837cecf7efb"],[{"index":{"value":1122},"size":{"value":92}},"004bf512-1b9a-4d95-8ca6-52ca0f8d061e"],[{"index":{"value":1101},"size":{"value":113}},"e65a17c1-b1a4-494f-b463-41a7c0578cce"],[{"index":{"value":1088},"size":{"value":126}},"71ab6a10-bd11-4305-8954-8fd159f3767e"],[{"index":{"value":1222},"size":{"value":4}},"e89e8b76-b1ef-405f-9b04-a5bae1abdc5e"],[{"index":{"value":1227},"size":{"value":3}},"c50e1f45-435f-4248-955b-34972d69ff36"],[{"index":{"value":1231},"size":{"value":8}},"8ca791ca-2c13-414f-9a15-4f23b1d830fc"],[{"index":{"value":1240},"size":{"value":4}},"04a7fa93-b458-461e-8f56-ad60fcf3ad56"],[{"index":{"value":1245},"size":{"value":4}},"8beb654f-a1b7-449c-8c16-6b0945877bb8"],[{"index":{"value":1250},"size":{"value":6}},"64bf0b90-be10-4c5e-8f1a-60fa86b156f6"],[{"index":{"value":1257},"size":{"value":5}},"a8f6e6d2-39a8-4c5c-8504-30ce3f14fa1a"],[{"index":{"value":1262},"size":{"value":1}},"753bdebb-f7b5-4de7-82b0-e6c9ed36e677"],[{"index":{"value":1264},"size":{"value":2}},"5c1817a4-72d4-4ada-b1d6-55f85874f6fe"],[{"index":{"value":1267},"size":{"value":7}},"759b2557-b042-430f-bed7-929606a454cf"],[{"index":{"value":1275},"size":{"value":5}},"2dc8261a-70b0-4515-827c-2eb17ec1a113"],[{"index":{"value":1281},"size":{"value":2}},"569c09ec-2ba5-4e1f-aecc-1d0dd2996808"],[{"index":{"value":1284},"size":{"value":3}},"8a9a8a0e-519a-453e-9e0c-5e48591c315c"],[{"index":{"value":1288},"size":{"value":5}},"eeb494c4-3093-45d2-93f6-dc3e1a72c1bf"],[{"index":{"value":1301},"size":{"value":7}},"e385bc66-3e08-492d-87e1-edfa50483341"],[{"index":{"value":1301},"size":{"value":8}},"0dfe206f-5ae6-4fda-9e73-8c1d9daa3da6"],[{"index":{"value":1293},"size":{"value":16}},"3643315c-ba7e-4d3e-be0a-17ae931c61b3"],[{"index":{"value":1219},"size":{"value":90}},"658a62b0-c13a-45ee-95dd-527475201db8"],[{"index":{"value":1314},"size":{"value":10}},"9e26e804-a343-4b2c-a5ce-e557d8ed33f2"],[{"index":{"value":1325},"size":{"value":1}},"c7cabbb3-ec1c-4e4d-84cd-05c2998e28de"],[{"index":{"value":1327},"size":{"value":9}},"bbd9fd86-28e3-4c26-b363-c939480cfa30"],[{"index":{"value":1336},"size":{"value":1}},"5cc48a22-5ead-4eae-8314-efb92530cf2f"],[{"index":{"value":1337},"size":{"value":4}},"17cf9c68-820d-4508-9972-aa2ba101f26b"],[{"index":{"value":1327},"size":{"value":14}},"e772ca0a-fda8-4d71-a408-fe49b573fbbb"],[{"index":{"value":1342},"size":{"value":10}},"16499f98-9c82-4372-8590-f35c9e40b20b"],[{"index":{"value":1327},"size":{"value":25}},"fa8c5b11-1144-4d5a-a71e-0a92b98739f8"],[{"index":{"value":1353},"size":{"value":9}},"a570a593-9876-4227-9516-337e94c88b78"],[{"index":{"value":1362},"size":{"value":1}},"f0c2a0c7-ef69-4abf-8bc6-2539a2a5507c"],[{"index":{"value":1363},"size":{"value":5}},"356088ff-9979-49a9-9489-0bcf30812235"],[{"index":{"value":1353},"size":{"value":15}},"2f460caa-6c22-4f1f-80f7-99304af3a078"],[{"index":{"value":1327},"size":{"value":41}},"11bf316e-7b92-4747-abc6-6da340efa199"],[{"index":{"value":1370},"size":{"value":13}},"ca1f7d0a-955b-4bc9-bca0-51e3d373fb13"],[{"index":{"value":1369},"size":{"value":15}},"100fd53b-6a0b-43ed-8a34-40d50b14136c"],[{"index":{"value":1327},"size":{"value":57}},"c146c417-2d5b-434b-a31d-633400d89c62"],[{"index":{"value":1314},"size":{"value":70}},"e4570dfe-fb88-4d71-9e6e-e4155c297b4a"],[{"index":{"value":1392},"size":{"value":4}},"428bde3f-957c-4b5b-85ba-a702cd74a002"],[{"index":{"value":1397},"size":{"value":3}},"9f1fca91-e924-41dc-be00-34e6a34e0508"],[{"index":{"value":1401},"size":{"value":9}},"8a5d6acd-b5ec-4b6e-aeed-4e2aa3f6942d"],[{"index":{"value":1411},"size":{"value":2}},"ac0056c6-09a7-4f4c-ac22-f14fc0fafa59"],[{"index":{"value":1414},"size":{"value":5}},"e45f7272-7398-41a0-9c85-c23494aafb15"],[{"index":{"value":1420},"size":{"value":8}},"275e23f1-375f-4b35-84c9-59b8ec27b15e"],[{"index":{"value":1429},"size":{"value":5}},"ae2c65cf-dae0-4870-996e-6ba4ecd489bb"],[{"index":{"value":1434},"size":{"value":1}},"4b7f3986-c2d6-4380-bfe2-e6c8c5154991"],[{"index":{"value":1436},"size":{"value":4}},"007f3e46-ec93-4dd4-8fc1-73633c14b7b6"],[{"index":{"value":1441},"size":{"value":3}},"70436ff7-f08d-4ccd-ad1f-f3c7bbf4ba43"],[{"index":{"value":1445},"size":{"value":4}},"c88a527e-50ca-4db5-81b2-eb3d8681fdad"],[{"index":{"value":1450},"size":{"value":8}},"f8c40d3f-d28f-44ee-9685-ef25e3927916"],[{"index":{"value":1466},"size":{"value":9}},"94797341-5a4f-4c4c-858b-46d36d9aa3fc"],[{"index":{"value":1476},"size":{"value":2}},"fd18485d-0f63-46e5-9965-0050037ff109"],[{"index":{"value":1466},"size":{"value":12}},"08ad3a47-098a-489a-b2a5-82ce68ce8471"],[{"index":{"value":1479},"size":{"value":3}},"8cc3d05e-2136-4f05-8cdb-23695c8ffa8b"],[{"index":{"value":1466},"size":{"value":16}},"57380b14-9355-4f10-9473-a15f7d6ff22c"],[{"index":{"value":1483},"size":{"value":5}},"fc2a9cb3-ec50-4824-b6bf-2f69e71d6c3d"],[{"index":{"value":1466},"size":{"value":22}},"354bfdb2-ede3-4122-a030-5d40b33c06d3"],[{"index":{"value":1489},"size":{"value":2}},"7a984ffc-5b85-40c6-9612-92e67ad389f6"],[{"index":{"value":1466},"size":{"value":25}},"e39fd2ca-519f-45fb-9c2d-dd692d584656"],[{"index":{"value":1492},"size":{"value":3}},"833298cc-a0ec-4fec-8355-2e224c798c41"],[{"index":{"value":1466},"size":{"value":29}},"06fcc623-6af6-4151-9ab9-e1e5a09760a0"],[{"index":{"value":1496},"size":{"value":5}},"f0aedf70-8cb3-43c6-809f-876656251c60"],[{"index":{"value":1496},"size":{"value":6}},"dcc494b0-9a16-45cd-be4f-17b352b6b22c"],[{"index":{"value":1466},"size":{"value":36}},"359a715a-2580-456a-8229-1af446607083"],[{"index":{"value":1458},"size":{"value":44}},"30a43ae2-e3d6-4c6e-a967-866112d763e0"],[{"index":{"value":1389},"size":{"value":113}},"5225ff45-e09e-4a29-94d4-2451be2a532f"],[{"index":{"value":1507},"size":{"value":10}},"ac024adb-cdc7-44a0-9426-ae16533e8ec6"],[{"index":{"value":1518},"size":{"value":1}},"ef7de61a-3c68-4c91-8d67-36b2d497e67d"],[{"index":{"value":1520},"size":{"value":10}},"1ab5e24d-e09d-4b65-896e-9597ed333b16"],[{"index":{"value":1530},"size":{"value":1}},"1f308d28-cfc4-42a5-bf9b-b8c1ea707657"],[{"index":{"value":1531},"size":{"value":8}},"c15afbd7-15c9-433b-b895-9f72065ae51f"],[{"index":{"value":1520},"size":{"value":19}},"0b25e76a-80f8-4d23-8230-de8a69f0a1f8"],[{"index":{"value":1541},"size":{"value":11}},"e08a7c82-6634-4442-b3bd-1ccbfeca4c75"],[{"index":{"value":1552},"size":{"value":1}},"cfac007b-1e34-437a-9cdd-a7ff27cbbac8"],[{"index":{"value":1553},"size":{"value":4}},"290df6b6-4aca-446d-9d5d-3f6076baeedd"],[{"index":{"value":1541},"size":{"value":16}},"c5a43d9b-c486-4c21-842e-b24e2d5e14b3"],[{"index":{"value":1558},"size":{"value":14}},"6eb4f8a4-03f8-429d-9e1e-b73bce2bb24f"],[{"index":{"value":1541},"size":{"value":31}},"7f725b52-3d75-461a-bc77-08cba3afbe6a"],[{"index":{"value":1573},"size":{"value":14}},"1bcac174-08f3-4af2-9f32-3c716ae9609e"],[{"index":{"value":1587},"size":{"value":1}},"8d66ef9c-0417-42b4-b727-64715148ef4c"],[{"index":{"value":1588},"size":{"value":10}},"f240d8cb-b811-4814-972d-a4395bd86262"],[{"index":{"value":1573},"size":{"value":25}},"d092a7b9-b84c-44d9-9f25-581bf6d63abe"],[{"index":{"value":1541},"size":{"value":57}},"51d4baac-d706-42e1-8e23-b9577cd9cb8a"],[{"index":{"value":1540},"size":{"value":59}},"5e45c320-15e8-402d-80d2-6935b9e07b32"],[{"index":{"value":1520},"size":{"value":79}},"28db4be8-e6ec-4bd5-962a-872c0ef59659"],[{"index":{"value":1507},"size":{"value":92}},"79f8f950-6ef2-4df3-b283-002730643af2"],[{"index":{"value":123},"size":{"value":1477}},"8b0e9c44-0478-4c6f-9012-e27125ae920c"],[{"index":{"value":117},"size":{"value":1483}},"7645a94d-3b35-4e76-b334-b622845c6445"],[{"index":{"value":0},"size":{"value":1600}},"3c5b7bad-b242-4d4a-9fd1-6700e4ff414c"]]
    {"ide":{"node":{"13f32da2-c06b-4a10-8941-20b11446eb81":{"position":{"vector":[-100.0,140.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"3d627933-6b6c-4a1a-97a1-cfb4a11d008f":{"position":{"vector":[-100.0,-640.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"1b99e619-d78d-40d6-ad30-f824c1b4cbbc":{"position":{"vector":[-356.03546,-417.26096]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"5fa7c5a0-15f4-446d-9322-bd5873a1b2cb":{"position":{"vector":[-328.01956,162.34082]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"849eb2a3-dedf-4298-a4da-805596b4a59f":{"position":{"vector":[-618.7991,49.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":{"name":{"content":{"content":"Table"}},"project":"Builtin"}},"a92f0102-ab00-4a51-80a1-b1b575667e64":{"position":{"vector":[-245.52425,-298.8625]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"c481c2e6-bfb0-428d-97c5-7f2aff608a8b":{"position":{"vector":[503.0,49.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"f203609a-a528-4d3f-bd06-4b53c2691fbc":{"position":{"vector":[-100.0,-460.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"6265c951-1d95-4e64-8520-b211ab326814":{"position":{"vector":[216.82806,-38.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"40222a66-01eb-419a-9595-2abd3ffd95b7":{"position":{"vector":[92.98981,-298.8625]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"64fa287f-e0e3-43b6-8ab2-2dd1925e2b67":{"position":{"vector":[489.70236,-38.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"81f24e32-f60c-494c-b10d-90007177f399":{"position":{"vector":[-73.0,-6.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"fa6ee8df-3d75-4822-a4d6-dfa5168b5f0b":{"position":{"vector":[503.0,-6.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":{"name":{"content":{"content":"Table"}},"project":"Builtin"}},"9744812f-608e-4b0c-8d1c-830528b99f49":{"position":{"vector":[-100.0,-340.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"fc47c83b-c770-4603-975b-f688b49ef5d0":{"position":{"vector":[198.58643,-674.85767]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":{"name":{"content":{"content":"Table"}},"project":"Builtin"}},"63ae4e2c-f5ab-4828-a24a-da462d49d3f7":{"position":{"vector":[41.94168,-174.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"28db4be8-e6ec-4bd5-962a-872c0ef59659":{"position":{"vector":[-412.0,-358.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":{"name":{"content":{"content":"Table"}},"project":"Builtin"}},"1213fb7c-dbb6-47a0-984b-0a4cbaa8cacd":{"position":{"vector":[371.0808,-108.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"3e019f4a-e800-488b-a359-0016fa479643":{"position":{"vector":[-57.999054,40.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"ab238dcf-c234-46b3-b20b-de9daf667abf":{"position":{"vector":[216.82806,94.915955]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"00c7a5cb-f480-4733-a549-f300dde92c61":{"position":{"vector":[-102.406876,-529.25287]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"9eeada36-9101-4e5e-a256-76f1ac767831":{"position":{"vector":[-100.0,-700.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"b2c0c249-6121-4f42-bfaf-9b2fcd15676e":{"position":{"vector":[-100.0,-520.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"6b1a5cd6-fb15-4a82-9bd4-e757f13e8111":{"position":{"vector":[188.22638,-600.50964]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"c5f78021-acd1-4849-be91-5809046e820a":{"position":{"vector":[-100.0,-580.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"ca8849f1-685e-40e4-8194-05db1259bfb2":{"position":{"vector":[321.1324,-91.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"e65a17c1-b1a4-494f-b463-41a7c0578cce":{"position":{"vector":[-103.0,-240.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"b804a267-ef59-47e5-a457-81b3395d7214":{"position":{"vector":[-428.0156,18.353895]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"79ef6943-fbec-4b60-add3-c0ffd2e2fe8a":{"position":{"vector":[126.63553,175.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"aa4af97b-5f21-43be-95e5-0e3a96a949c8":{"position":{"vector":[-816.77,162.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"b5c67b94-9387-4b58-8fc8-06b2ef862da8":{"position":{"vector":[-761.15936,265.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"fcd0e694-3334-4f14-98f0-e1afe30075a1":{"position":{"vector":[-100.0,-280.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"c0236d2f-2e37-439d-8aaa-a1bc7fa68f7d":{"position":{"vector":[-399.0,163.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"8ec06220-e1e8-41e7-8105-2cd5a2bd4b3e":{"position":{"vector":[-102.406876,-469.2529]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"da0e81da-f85b-4686-bd5c-2a3d3b02794f":{"position":{"vector":[-156.0,95.01155]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"f6b10179-e443-4ede-8d85-de95027f84b8":{"position":{"vector":[48.94168,30.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"8aa65bb9-296e-4ef8-85e1-eda830ad992d":{"position":{"vector":[-157.43556,-358.8625]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"6f7a10b4-e9a5-45c5-a131-b824c5f60dce":{"position":{"vector":[-73.0,49.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"91214607-74db-455c-8904-04e5fa10f820":{"position":{"vector":[-100.0,-400.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"699894a4-6d87-420e-9697-98e75ef5c165":{"position":{"vector":[-353.6286,-212.69028]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"c146c417-2d5b-434b-a31d-633400d89c62":{"position":{"vector":[-412.0,-298.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null},"e2c91a90-6712-4176-8340-dfc0d6df3a13":{"position":{"vector":[-100.0,-220.0]},"intended_method":null,"uploading_file":null,"selected":false,"visualization":null}},"import":{},"project":null}}
    "#;
            let ast = Parser::new().parse_module(code, default()).unwrap();
            assert_eq!(ast.repr(), code);
        }
         */

    #[test]
    fn test_orders_repr() {
        let code = r#"
from Standard.Base import all
from Standard.Table import all
import Standard.Visualization
import Standard.Examples

main =
    ## The file contains three different sheets relating to operations of an
       online store.
    operator2 = enso_project.data / 'store_data.xlsx'
    ## Read the customers table.
    operator3 = operator2.read (Excel (Worksheet 'Customers'))
    ## Read the products table.
    operator4 = operator2.read (Excel (Worksheet 'Items'))
    ## Read the orders history.
    operator5 = operator2.read (Excel (Worksheet 'Orders'))
    ## Join the item data to the order history, to get information on item
       prices in the orders table.
    operator8 = operator5.join operator4  Join_Kind.Inner ['Item ID']
    operator1 = operator8.at 'Unit Price'
    operator9 = operator8.at 'Quantity'
    ## Multiply item prices and counts to get total order value.
    product1 = operator1 * operator9
    operator10 = operator8.set 'Order Value' product1
    ## Group all orders by the Customer ID, to compute the total value of orders
       placed by each client.
    operator11 = operator10.aggregate [Aggregate_Column.Group_By 'Customer ID', Aggregate_Column.Sum 'Order Value' 'Orders Value']
    ## Join the customer data into orders table, to include names in the final
       ranking.
    operator16 = operator3.join operator11 Join_Kind.Inner ["Customer ID"]
    ## Sort the customers by their lifetime value, with the most valuable
       customers at the start of the table.
    operator17 = operator16.order_by (Sort_Column.Name 'Orders Value' Sort_Direction.Descending)
"#;
        let ast = Parser::new().parse_module(code, default()).unwrap();
        assert_eq!(ast.repr(), code);
    }

    #[test]
    fn test_as_lambda() {
        let ast = Parser::new().parse_line_ast("a->4").unwrap();
        assert!(ast::macros::as_lambda(&ast).is_some(), "{:?}", ast);
    }
}
