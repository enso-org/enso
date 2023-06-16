Here are people who have contributed to the development of Jackson JSON processor
databind core component, version 2.x
(version numbers in brackets indicate release in which the problem was fixed)

(note: for older credits, check out release notes for 1.x versions)

Tatu Saloranta, tatu.saloranta@iki.fi: author

Pascal Glinas:
  * Contributed fixes to 'MappingIterator' handling (Pull#58 and Pull#59)
   (2.1.0)
  * Reported #220: ContainerNode missing 'createNumber(BigInteger)'
   (2.2.2)

Joern Huxhorn: (huxi@github)
  * Suggested [JACKSON-636]: Add 'SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS' to allow
    forced sorting of Maps during serialization
   (2.0.0)
  * Reported #479: NPE on trying to deserialize a `String[]` that contains null
   (2.4.1)
  * Reported #1411: MapSerializer._orderEntries should check for null keys
   (2.7.9)

James Roper:
 * Requested [JACKSON-732]: Allow 'AnnotationIntrospector.findContentDeserializer()'
    (and similar) to return instance, not just Class<?> for instance
  (2.0.0)
 * Suggested [JACKSON-800]: Adding a method for letting modules register
    DeserializationProblemHandlers
  (2.0.0)

Casey Lucas:
 * Reported [JACKSON-798]: Problem with external type id, creators
  (2.0.0)

Tammo van Lessen:
 * Reported [JACKSON-811]: Problems with @JsonIdentityInfo, abstract types
  (2.0.0)
 * Reported [JACKSON-814]: Parsing RFC822/RFC1123 dates failes on non-US locales
  (2.0.0)

Raymond Myers:
 * Suggested [JACKSON-810]: Deserialization Feature: Allow unknown Enum values via
    'DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL'
  (2.0.0)

Ryan Gardner:
 * Contributed #5 -- Add support for maps with java.util.Locale keys
    to the set of StdKeyDeserializers
  (2.0.1)

Razvan Dragut:
 * Suggested [JACKSON-850]: Allow use of zero-arg factory methods as "default creator"
  (2.1.0)

Duncan Atkinson:
 * Reported [JACKSON-851]: State corruption with ObjectWriter, DefaultPrettyPrinter
  (2.1.0)

Mark Wolfe:
 * Suggested #45: Add `@JsonNaming()` for per-class naming strategy overrides
  (2.1.0)

Dmitry Katsubo:
 * Contributed patch for #65: Add getters to `ObjectMapper`, DeserializationContext,
   DeserializationFactory.
  (2.1.0)

Francis Galiegue:
 * Reported #93 (and suggested fix): bug in `ObjectMapper.setAll(...)'
  implementation
  (2.1.1)
 * Reported #433: `ObjectMapper`'s `.valueToTree()` wraps `JsonSerializable` objects
  into a POJONode
  (2.3.3)
 * Contributed #434: Ensure that DecimalNodes with mathematically equal values are equal
  (2.4.0)

kelaneren@github:
 * Reported #157, contributed unit test: NPE when registering same module twice.
  (2.1.4)

Eric Tschetter (cheddar@github):
  * Reported issues #166, #167, #170 (regressions from 1.9.x to 2.x)
   (2.1.4)

Thierry D (thierryd@github)
  * Reported #214: Problem with LICENSE, NOTICE, Android packaging
   (2.2.2)

Luke G-H (lukegh@github)
  * Reported #223: Duplicated nulls with @JsonFormat(shape=Shape.ARRAY)
   (2.2.2)

Karl Moore (karldmoore@github)
  * Reported #217: JsonProcessingExceptions not all wrapped as expected
   (2.2.2)

David Phillips:
  * Requested #308: Improve serialization and deserialization speed of `java.util.UUID`
   (2.3.0)

Seth Pellegrino (jivesoft):
  * Contributed #317: Fix `JsonNode` support for nulls bound to	`ObjectNode`, `ArrayNode`
   (2.3.0)

Florian Schoppmann (fschopp@github)
  * Reported #357: StackOverflowError with contentConverter that returns array type
   (2.7.0)
  * Reported #358: `IterableSerializer` ignoring	annotated content serializer
   (2.3.1)
  * Reported #359: Converted object not using explicitly annotated serializer
   (2.4.0)

Martin Traverso:
  * Reported #406: Cannot use external type id + @JsonTypeIdResolver
   (2.3.2)

Matthew Morrissette:
  * Contributed #381: Allow inlining/unwrapping of value from single-component JSON array
   (2.4.0)

Will Palmeri: (wpalmeri@github)
  * Contributed #407: Make array and Collection serializers use configured value null handler
   (2.4.0)

Cemalettin Koc: (cemo@github)
  * Reported #353: Problems with polymorphic types, `JsonNode` (related to #88)
   (2.4.0)

Ben Fagin: (UnquietCode@github)
  * Suggested #442: Make `@JsonUnwrapped` indicate property inclusion
   (2.4.0)
  * Contributed #81/#455: Allow use of @JsonUnwrapped with typed (@JsonTypeInfo) classes,
    provided that (new) feature `SerializationFeature.FAIL_ON_UNWRAPPED_TYPE_IDENTIFIERS`
    is disabled
   (2.4.0)

Chris Cleveland:
  * Suggested #463: Add 'JsonNode.asText(String defaultValue)`
   (2.4.0)

Benson Margulies:
  * Reported #467: Unwanted POJO's embedded in tree via serialization to tree
   (2.4.0)
  * Reported #601: ClassCastException for a custom serializer for enum key in `EnumMap`
   (2.4.4)
  * Contributed 944: Failure to use custom deserializer for key deserializer
   (2.6.3)
  * Reported #1120: String value omitted from weirdStringException
   (2.6.6)
  * Reported, fixed #1235: `java.nio.file.Path` support incomplete
   (2.8.0)
  * Reported #1270: Generic type returned from type id resolver seems to be ignored
   (2.8.0)

Steve Sanbeg: (sanbeg@github)
  * Contributed #482: Make date parsing error behavior consistent with JDK
   (2.4.1)

Ian Barfield: (tea-dragon@github)
  * Reported #580: delegate deserializers choke on a (single) abstract/polymorphic parameter
   (2.4.4)
  * Reported #844: Using JsonCreator still causes invalid path references in JsonMappingException
   (2.5.5)

Eugene Lukash
  * Reported #592: Wrong `TokenBuffer` delegate deserialization using `@JsonCreator`
   (2.4.4)

Fernando Otero (zeitos@github)
  * Contributed fix for #610: Problem with forward reference in hierarchies
   (2.4.4)

Lovro Pandžić (lpandzic@github)
  * Reported #421: @JsonCreator not used in case of multiple creators with parameter names
   (2.5.0)
  * Requested #1498: Allow handling of single-arg constructor as property based by default
   (2.12.0)

Adam Stroud (adstro@github)
  * Contributed	#576: Add fluent API for adding mixins
   (2.5.0)

David Fleeman (fleebytes@github)
  * Contributed #528 implementation: Add support for `JsonType.As.EXISTING_PROPERTY`
   (2.5.0)

Aurélien Leboulanger (herau@github)
  * Contributed improvement for #597: Improve error messaging for cases	where JSON Creator
    returns null (which is illegal)
   (2.5.0)

Michael Spiegel (mspiegel@githib)
  * Contributed #636: `ClassNotFoundException` for classes not (yet) needed during serialization
   (2.5.0)

Michael Ressler (mressler@github)
  * Contributed #566: Add support for case-insensitive deserialization
   (`MapperFeature.ACCEPT_CASE_INSENSITIVE_PROPERTIES`)
   (2.5.0)

Konstantin Labun (kulabun@github)
  * Reported #647: Deserialization fails when @JsonUnwrapped property contains an object with same property name
   (2.5.0)

Christopher Smith (chrylis@github)
  * Reported #594: `@JsonValue` on enum not used when enum value is a Map key
   (2.5.0)

Alexandre Santana Campelo (alexqi200@github):
  * Contributed #671: Adding `java.util.Currency` deserialization support for maps
   (2.5.1)

Zoltan Farkas (zolyfarkas@github)
  * Reported #674: Spring CGLIB proxies not handled as intended
   (2.5.1)

Ludevik@github:
  * Reported #682: Class<?>-valued Map keys not serialized properly
   (2.5.1)

Antibrumm@github:
  * Reported #691: Jackson 2.5.0. NullSerializer for MapProperty failing
   (2.5.2)
  * Reported #984: JsonStreamContexts are not build the same way for write.. and convert methods
   (2.6.4)

Shumpei Akai (flexfrank@github)
  * Reported #703: Multiple calls to ObjectMapper#canSerialize(Object.class) returns different values
   (2.5.2)

Francisco A. Lozano (flozano@github)
  * Contributed fix for #703 (see above)
   (2.5.2)

Dylan Scott (dylanscott@github)
  * Reported #738: #738: @JsonTypeInfo non-deterministically ignored in 2.5.1 (concurrency
    issue)
   (2.5.2)

Alain Gilbert (agilbert314@github)
  * Reporter, contributed #766: Fix Infinite recursion (StackOverflowError) when
    serializing a SOAP object
   (2.5.3)

Alexey Gavrilov (Alexey1Gavrilov@github)
  * Reported, contributed fix for #761: Builder deserializer: in-compatible type exception
    when return type is super type
   (2.5.3)

Dmitry Spikhalskiy (Spikhalskiy@github)
  * Reported #731, suggested the way to fix it: XmlAdapter result marshaling error in
    case of ValueType=Object
   (2.5.3)
  * Reported #1456: `TypeFactory` type resolution broken in 2.7 for generic types
   when using `constructType` with context
   (2.7.9 / 2.8.6)

John Meyer (jpmeyer@github)
  * Reported, contributed fix for #745: EnumDeserializer.deserializerForCreator() fails
    when used to deserialize a Map key
   (2.5.3)

Andrew Duckett (andrewduckett@github)
  * Reported #771: Annotation bundles ignored when added to Mixin
   (2.5.4)

Charles Allen:
  * Contributed #785: Add handlings for classes which are available in
    `Thread.currentThread().getContextClassLoader()`
   (2.5.4)

Andrew Goodale (newyankeecodeshop@github)
  * Contributed #816: Allow date-only ISO strings to have no time zone
   (2.5.4)

Kamil Benedykciński (Kamil-Benedykcinski@github)
  * Contributed #801: Using `@JsonCreator` cause generating invalid path reference
   in `JsonMappingException`
   (2.5.4)

Chi Kim (chikim79@github)
  * Reported #878: serializeWithType on BeanSerializer does not setCurrentValue
   (2.5.5 / 2.6.1)

Charles Allen (drcrallen@github):
  * Reported #696: Copy constructor does not preserve `_injectableValues`
   (2.6.0)

Chris Pimlott (pimlottc@github):
  * Suggested #348: ObjectMapper.valueToTree does not work with @JsonRawValue
   (2.6.0)

Laird Nelson (ljnelson@github)
  * Suggested #688: Provide a means for an ObjectMapper to discover mixin annotation
    classes on demand
   (2.6.0)
  * Reported #1088: NPE possibility in SimpleMixinResolver
   (2.6.6)

Derk Norton (derknorton@github)
  * Suggested #689: Add `ObjectMapper.setDefaultPrettyPrinter(PrettyPrinter)`
   (2.6.0)

Michal Letynski (mletynski@github)
  * Suggested #296: Serialization of transient fields with public getters (add
    MapperFeature.PROPAGATE_TRANSIENT_MARKER)
   (2.6.0)

Jeff Schnitzer (stickfigure@github)
  * Suggested #504: Add `DeserializationFeature.USE_LONG_FOR_INTS`
   (2.6.0)

Jerry Yang (islanderman@github)
  * Contributed #820: Add new method for `ObjectReader`, to bind from JSON Pointer position
   (2.6.0)

Lars Pfannenschmidt (larsp@github)
  * Contributed #826: Replaced synchronized HashMap with ConcurrentHashMap in
   TypeDeserializerBase._findDeserializer
   (2.6.0)

Stephen A. Goss (thezerobit@github)
  * Contributed #828: Respect DeserializationFeatures.WRAP_EXCEPTIONS in CollectionDeserializer
   (2.6.0)

Andy Wilkinson (wilkinsona@github)
  * Reported #889: Configuring an ObjectMapper's DateFormat changes time zone
   (2.6.1)

lufe66@github:
  * Reported 894: When using withFactory on ObjectMapper, the created Factory has a TypeParser
    which still has the original Factory
   (2.6.2)

Daniel Walker (dsw2127@github)
  * Reported, contributed fix for #913: `ObjectMapper.copy()` does not preserve
   `MappingJsonFactory` features
   (2.6.2)

Sadayuki Furuhashi (frsyuki@github)
  * Reported #941: Deserialization from "{}" to ObjectNode field causes
    "out of END_OBJECT token" error
   (2.6.3)
  * Reported #2077: `JsonTypeInfo` with a subtype having `JsonFormat.Shape.ARRAY`
    and no fields generates `{}` not `[]`
   (2.10.0)

David Haraburda (dharaburda@github)
  * Contributed #918: Add `MapperFeature.ALLOW_EXPLICIT_PROPERTY_RENAMING`
   (2.7.0)

Sergio Mira (Sergio-Mira@github)
  * Contributed #940: Add missing `hashCode()` implementations for `JsonNode` types that did not have them
   (2.6.3)

Andreas Pieber (anpieber@github)
  * Reported #939: Regression: DateConversionError in 2.6.x	
   (2.6.3)

Jesse Wilson (swankjesse@github)
  * Contributed #948: Support leap seconds, any number of millisecond digits for ISO-8601 Dates.
   (2.6.3)
  * Contributed #949: Report the offending substring when number parsing fails
   (2.6.3)

Warren Bloomer (stormboy@github)
  * Reported #942: Handle null type id for polymorphic values that use external type id
   (2.6.3)

Ievgen Pianov (pyanoveugen@github)
  * Reported #989: Deserialization from "{}" to java.lang.Object causes "out of END_OBJECT token" error
   (2.6.3)

Jayson Minard (apatrida@github)
  * Reported #1005: Synthetic constructors confusing Jackson data binding
   (2.6.4)
  * Reported #1438: `ACCEPT_CASE_INSENSITIVE_PROPERTIES` is not respected for creator properties
   (2.8.5)

David Bakin (david-bakin@github)
* Reported #1013: `@JsonUnwrapped` is not treated as assuming `@JsonProperty("")`
   (2.6.4)
  * Suggested #1011: Change ObjectWriter::withAttributes() to take a Map with some kind of wildcard types
   (2.7.0)
  * Reported #962: `@JsonInject` fails on trying to find deserializer even if inject-only
   (2.11.0)

Dmitry Romantsov (DmRomantsov@github)
  * Reported #1036: Problem with case-insensitive deserialization
   (2.6.4)

Daniel Norberg (danielnorberg@github)
  * Contributed #1099: Fix custom comparator container node traversal
   (2.6.6)

Miles Kaufmann (milesk-amzn@github)
  * Reported #432: `StdValueInstantiator` unwraps exceptions, losing context
   (2.7.0)

Thomas Mortagne (tmortagne@github)
  * Suggested #857: Add support for java.beans.Transient
   (2.7.0)

Jonas Konrad (yawkat@github)
  * Suggested #905: Add support for `@ConstructorProperties`
   (2.7.0)

Jirka Kremser (Jiri-Kremser@github)
  * Suggested #924: SequenceWriter.writeAll() could accept Iterable
   (2.7.0)

Daniel Mischler (danielmischler@github)
  * Requested #963: Add PropertyNameStrategy `KEBAB_CASE`
   (2.7.0)

Shumpei Akai (flexfrank@github)
  * Reported #978: ObjectMapper#canSerialize(Object.class) returns false even though
   FAIL_ON_EMPTY_BEANS is disabled
   (2.7.0)

Hugo Wood (hgwood@github)
  * Contributed #1010: Support for array delegator
   (2.7.0)

Julian Hyde (julianhyde@github)
  * Reported #1083: Field in base class is not recognized, when using `@JsonType.defaultImpl`
   (2.7.1)

Thibault Kruse (tkruse@github)
  * Reported #1102: Handling of deprecated `SimpleType.construct()` too minimalistic
   (2.7.1)

Aleks Seovic (aseovic@github)
  * Reported #1109: @JsonFormat is ignored by the DateSerializer unless either a custom pattern
    or a timezone are specified
   (2.7.1)

Timur Shakurov (saladinkzn@github)
  * Reported #1134: Jackson 2.7 doesn't work with jdk6 due to use of `Collections.emptyIterator()`
   (2.7.2)

Jiri Mikulasek (pirkogdc@github)
  * Reported #1124: JsonAnyGetter ignores JsonSerialize(contentUsing=...)
   (2.7.2)

Xavi Torrens (xavitorrens@github)
  * Reported #1150: Problem with Object id handling, explicit `null` token
   (2.7.3)

Yoann Rodière (fenrhil@github)
  * Reported #1154: @JsonFormat.pattern on dates is now ignored if shape is not
    explicitely provided
   (2.7.3)

Mark Woon (markwoon@github)
  * Reported #1178: `@JsonSerialize(contentAs=superType)` behavior disallowed in 2.7
   (2.7.4)
  * Reported #1231: `@JsonSerialize(as=superType)` behavior disallowed in 2.7.4
   (2.7.5)
  * Suggested #507: Support for default `@JsonView` for a class
   (2.9.0)

Tom Mack (tommack@github)
  * Reported #1208: treeToValue doesn't handle POJONodes that contain exactly
    the requested value type
   (2.7.4)

William Headrick (headw01@github)
   * Reported#1223: `BasicClassIntrospector.forSerialization(...).findProperties` should
    respect MapperFeature.AUTO_DETECT_GETTERS/SETTERS?
   (2.7.5)

Nick Babcock (nickbabcock)
  * Reported #1225: `JsonMappingException` should override getProcessor()
   (2.7.5)
  * Suggested #1356: Differentiate between input and code exceptions on deserialization
   (2.9.0)

Andrew Joseph (apjoseph@github)
  * Reported #1248: `Annotated` returns raw type in place of Generic Type in 2.7.x
   (2.7.5)

Erich Schubert (kno10@github)
  * Reported #1260: `NullPointerException` in `JsonNodeDeserializer`, provided fix
   (2.7.5)

Brian Pontarelli (voidmain@github)
  * Reported #1301: Problem with `JavaType.toString()` for recursive (self-referential) types
   (2.7.6)

Max Drobotov (fizmax@github)
  * Reported, contributed fix for #1332: `ArrayIndexOutOfBoundException` for enum by index deser
   (2.7.7)

Stuart Douglas (stuartwdouglas@github)
  * Reported #1363: The static field ClassUtil.sCached can cause a class loader leak
   (2.7.8)

Josh Caplan (jecaplan@github)
  * Reported, suggested fix for #1368: Problem serializing `JsonMappingException` due to addition
    of non-ignored `processor` property (added in 2.7)
   (2.7.8)

Diego de Estrada (diegode@github)
  * Contributed fix for #1367: No Object Id found for an instance when using `@ConstructorProperties`
   (2.7.9)

Kevin Hogeland (khogeland@github)
  * Reported #1501: `ArrayIndexOutOfBoundsException` on non-static inner class constructor
   (2.7.9)

xiexq (xiexq@knownsec.com)
  * Reported #2389: Block one more gadget type (CVE-2019-14361)
   (2.7.9.6)

Artur Jonkisz (ajonkisz@github)
  * Reported #960: `@JsonCreator` not working on a factory with no arguments for ae enum type
   (2.8.0)

Mikhail Kokho (mkokho@github)
  * Contributed impl for #990: Allow failing on `null` values for creator (add
  `DeserializationFeature.FAIL_ON_NULL_CREATOR_PROPERTIES`)
   (2.8.0)

Aleksandr Oksenenko (oleksandr-oksenenko@github)
  * Reported #999: External property is not deserialized
   (2.8.0)

Lokesh Kumar (LokeshN@github)
  * Contributed impl for #1082: Can not use static Creator factory methods for `Enum`s,
    with JsonCreator.Mode.PROPERTIES
   (2.8.0)
  * Reported #1217: `@JsonIgnoreProperties` on Pojo fields not working for deserialization
   (2.8.0)

Ross Goldberg
  * Reported #1165, provided fix for: `CoreXMLDeserializers` does not handle
    time-only `XMLGregorianCalendar`s
   (2.8.0)

Maarten Billemont (lhunath@github)
  * Suggested #1184: Allow overriding of `transient` with explicit inclusion with `@JsonProperty`
   (2.8.0)

Vladimir Kulev (lightoze@github)
  * Reported #1028: Ignore USE_BIG_DECIMAL_FOR_FLOATS for NaN/Infinity
   (2.8.0)

Ari Fogel (arifogel@github)
  * Reported #1261, contributed fix for: `@JsonIdentityInfo` deserialization fails with
    combination of forward references, `@JsonCreator`
   (2.8.0)

Andriy Plokhotnyuk (plokhotnyuk@github)
  * Requested #1277: Add caching of resolved generic types for `TypeFactory`
   (2.8.0)

Arek Gabiga (arekgabiga@github)
  * Reported #1297: Deserialization of generic type with Map.class
   (2.8.1)

Chris Jester-Young (cky@github)
  * Contributed #1335: Unconditionally call `TypeIdResolver.getDescForKnownTypeIds`
   (2.8.2)

Andrew Snare (asnare@github)
  * Reported #1315: Binding numeric values can BigDecimal lose precision
   (2.8.2)

Gili Tzabari (cowwoc@github)
  * Reported #1351: `@JsonInclude(NON_DEFAULT)` doesn't omit null fields
   (2.8.3)

Oleg Zhukov (OlegZhukov@github)
  * Reported #1384: `@JsonDeserialize(keyUsing = ...)` does not work correctly
   together with `DefaultTyping.NON_FINAL`
   (2.8.4)

Pavel Popov (tolkonepiu@github)
  * Contributed fix #1389: Problem with handling of multi-argument creator with Enums
   (2.8.4)

Josh Gruenberg (joshng@github)
  * Reported #1403: Reference-chain hints use incorrect class-name for inner classes
   (2.8.4)

Kevin Donnelly (kpdonn@github)
  * Reported #1432: Off by 1 bug in PropertyValueBuffer
   (2.8.5)

Nathanial Ofiesh (ofiesh@github)
  * Reported #1441: Failure with custom Enum key deserializer, polymorphic types
   (2.8.5)

Frédéric Camblor (fcamblor@github)
  * Reported #1451: Type parameter not passed by `ObjectWriter` if serializer pre-fetch disabled
   (2.8.6)

Stephan Schroevers (Stephan202@github)
  * Reported #1505: @JsonEnumDefaultValue should take precedence over FAIL_ON_NUMBERS_FOR_ENUMS
   (2.8.7)

Alex Panchenko (panchenko@github)
  * Reported #1543: JsonFormat.Shape.NUMBER_INT does not work when defined on enum type in 2.8
   (2.8.8)

Joshua Jones
  * Reported #1573, contributed fix: Missing properties when deserializing using a builder class
   with a non-default constructor and a mutator annotated with `@JsonUnwrapped`
   (2.8.8)

Ivo Studens (istudens@redhat.com)
  * Contributed #1585: Invoke ServiceLoader.load() inside of a privileged block
    when loading modules using `ObjectMapper.findModules()`
   (2.8.9)
  * Contributed fix for #2482: `JSONMappingException` `Location` column number
    is one line Behind the actual location
   (2.10.3)

Javy Luo (AnywnYu@github)
  * Reported #1595: `JsonIgnoreProperties.allowSetters` is not working in Jackson 2.8
   (2.8.9)

Marco Catania (catanm@github.com)
  * Contributed #1597: Escape JSONP breaking characters
   (2.8.9)

Andrew Joseph (apjoseph@github)
  * Reported #1629 `FromStringDeserializer` ignores registered `DeserializationProblemHandler`
    for `java.util.UUID`
   (2.8.9)

Joe Littlejohn (joelittlejohn@github)
  * Contributed #1642: Support `READ_UNKNOWN_ENUM_VALUES_AS_NULL` with `@JsonCreator`
   (2.8.9)

Slobodan Pejic (slobo-showbie@github)
  * Reported #1647, contributed fix: Missing properties from base class when recursive
    types are involved
   (2.8.9)

Bertrand Renuart (brenuart@github)
  * Reported #1648: `DateTimeSerializerBase` ignores configured date format when creating contextual
   (2.8.9)
  * Reported #1651: `StdDateFormat` fails to parse 'zulu' date when TimeZone other than UTC
   (2.8.9)
  * Suggested #1745: StdDateFormat: accept and truncate millis larger than 3 digits
   (2.9.1)
  * Contributed #1749: StdDateFormat: performance improvement of '_format(..)' method
   (2.9.1)
  * Contributed #1759: Reuse `Calendar` instance during parsing by `StdDateFormat`
   (2.9.1)

Kevin Gallardo (newkek@github)
  * Reported #1658: Infinite recursion when deserializing a class extending a Map,
    with a recursive value type
   (2.8.10)
  * Reported #1729: Integer bounds verification when calling `TokenBuffer.getIntValue()`
   (2.9.4)

Lukas Euler
  * Reported #1735: Missing type checks when using polymorphic type ids

Guixiong Wu (吴桂雄)
  * Reported #2032: Blacklist another serialization gadget (ibatis)
   (2.8.11.2)

svarzee@github
  * Reported #2109, suggested fix: Canonical string for reference type is built incorrectly
   (2.8.11.3 / 2.9.7)

Connor Kuhn (ckuhn@github)
  * Contributed #1341: FAIL_ON_MISSING_EXTERNAL_TYPE_ID_PROPERTY
   (2.9.0)

Jan Lolling (jlolling@github)
  * Contributed #1319: Add `ObjectNode.put(String, BigInteger)`
   (2.9.0)

Michael R Fairhurst (MichaelRFairhurst@github)
  * Reported #1035: `@JsonAnySetter` assumes key of `String`, does not consider declared type.
   (2.9.0)

Fabrizio Cucci (fabriziocucci@github)
  * Reported #1406: `ObjectMapper.readTree()` methods do not return `null` on end-of-input
   (2.9.0)

Emiliano Clariá (emilianogc@github)
  * Contributed #1434: Explicitly pass null on invoke calls with no arguments
   (2.9.0)

Ana Eliza Barbosa (AnaEliza@github)
  * Contributed #1520: Case insensitive enum deserialization feature.
   (2.9.0)

Lyor Goldstein (lgoldstein@github)
  * Reported #1544: `EnumMapDeserializer` assumes a pure `EnumMap` and does not support
    derived classes
   (2.9.0)

Harleen Sahni (harleensahni@github)
  * Reported #403: Make FAIL_ON_NULL_FOR_PRIMITIVES apply to primitive arrays and other
    types that wrap primitives
   (2.9.0)

Jared Jacobs (2is10@github)
  * Requested #1605: Allow serialization of `InetAddress` as simple numeric host address
   (2.9.0)

Patrick Gunia (pgunia@github)
  * Reported #1440: Wrong `JsonStreamContext` in `DeserializationProblemHandler` when reading
  `TokenBuffer` content
   (2.9.0)

Carsten Wickner (CarstenWickner@github)
  * Contributed #1522: Global `@JsonInclude(Include.NON_NULL)` for all properties with a specific type
   (2.9.0)

Chris Plummer (strmer15@github)
  * Reported #1637: `ObjectReader.at()` with `JsonPointer` stops after first collection
   (2.9.0)

Christian Basler (Dissem@github)
  * Reported #1688: Deserialization fails for `java.nio.file.Path` implementations when
    default typing enabled
   (2.9.0)

Tim Bartley (tbartley@github)
  * Reported, suggested fix for #1705: Non-generic interface method hides type resolution info
    from generic base class
   (2.9.1)

Luís Cleto (luiscleto@github)
  * Suggested 1768: Improve `TypeFactory.constructFromCanonical()` to work with
   `java.lang.reflect.Type.getTypeName()` format
   (2.9.2)

Vincent Demay (vdemay@github)
  * Reported #1793: `java.lang.NullPointerException` in `ObjectArraySerializer.acceptJsonFormatVisitor()`
    for array value with `@JsonValue`
   (2.9.2)

Peter Jurkovic (peterjurkovic@github)
  * Reported #1823: ClassNameIdResolver doesn't handle resolve Collections$SingletonMap,
    Collections$SingletonSet
   (2.9.3)

alinakovalenko@github:
  * Reported #1844: Map "deep" merge only adds new items, but not override existing values
   (2.9.3)

Pier-Luc Whissell (pwhissell@github):
  * Reported #1673: Serialising generic value classes via Reference Types (like Optional) fails
    to include type information
   (2.9.4)

Alexander Skvortcov (askvortcov@github)
  * Reported #1853: Deserialise from Object (using Creator methods) returns field name
    instead of value
   (2.9.4)

Joe Schafer (jschaf@github)
  * Reported #1906: Add string format specifier for error message in `PropertyValueBuffer`
   (2.9.4)
  * Reported #1907: Remove `getClass()` from `_valueType` argument for error reporting
   (2.9.4)

Deblock Thomas (deblockt@github)
  * Reported, contributed fix for #1912: `BeanDeserializerModifier.updateBuilder()` does not
    work to set custom  deserializer on a property (since 2.9.0)
   (2.9.5)
  * Reported, suggested fix for #2280: JsonMerge not work with constructor args
   (2.10.0)

lilei@venusgroup.com.cn:
  * Reported #1931: Two more `c3p0` gadgets to exploit default typing issue
   (2.9.5)

Aniruddha Maru (maroux@github)
  * Reported #1940: `Float` values with integer value beyond `int` lose precision if
    bound to `long`
   (2.9.5)

Timur Shakurov (saladinkzn@github)
  * Reported #1947: `MapperFeature.AUTO_DETECT_XXX` do not work if all disabled
   (2.9.5)

roeltje25@github
  * Reported #1978: Using @JsonUnwrapped annotation in builderdeserializer hangs in
    infinite loop
   (2.9.5)

Freddy Boucher (freddyboucher@github)
  * Reported #1990: MixIn `@JsonProperty` for `Object.hashCode()` is ignored
   (2.9.6)

Ondrej Zizka (OndraZizk@github)
  * Reported #1999: "Duplicate property" issue should mention which class it complains about
   (2.9.6)

Jakub Skierbiszewski (jskierbi@github)
  * Reported, contributed fix for #2001: Deserialization issue with `@JsonIgnore` and
    `@JsonCreator` + `@JsonProperty` for same property name
   (2.9.6)

Carter Kozak (cakofony@github)
  * Reported #2016: Delegating JsonCreator disregards JsonDeserialize info
   (2.9.6)

Reinhard Prechtl (dnno@github)
  * Reported #2034: Serialization problem with type specialization of nested generic types
   (2.9.6)

Chetan Narsude (243826@github)
  * Reported #2038: JDK Serializing and using Deserialized `ObjectMapper` loses linkage
    back from `JsonParser.getCodec()`
   (2.9.6)

Petar Tahchiev (ptahchiev@github)
  * Reported #2060: `UnwrappingBeanPropertyWriter` incorrectly assumes the found
    serializer is of type `UnwrappingBeanSerializer`
   (2.9.6)

Brandon Krieger (bkrieger@github)
  * Reported #2064: Cannot set custom format for `SqlDateSerializer` globally
   (2.9.7)

Thibaut Robert (trobert@github)
  * Requested #2059: Remove `final` modifier for `TypeFactory`
   (2.10.0)

Christopher Smith (chrylis@github)
  * Suggested #2115: Support naive deserialization of `Serializable` values as "untyped",
    same as `java.lang.Object`		     
   (2.10.0)

Édouard Mercier (edouardmercier@github)
  * Requested #2116: Make NumberSerializers.Base public and its inherited classes not final
   (2.9.6)

Semyon Levin (remal@github)
  * Contributed #2120: `NioPathDeserializer` improvement
   (2.9.7)
  * Contributed #2133: Improve `DeserializationProblemHandler.handleUnexpectedToken()`
    to allow handling of Collection problems
   (2.10.0)

Pavel Nikitin (morj@github)
  * Requested #2181: Don't re-use dynamic serializers for property-updating copy constructors
   (2.9.8)

Thomas Krieger (ThomasKrieger@github)
  * Reported #1408: Call to `TypeVariable.getBounds()` without synchronization unsafe on
    some platforms
   (2.9.9)

René Kschamer (flawi@github)
  * Reported #2197: Illegal reflective access operation warning when using `java.lang.Void`
   as value type
   (2.9.8)

Joffrey Bion (joffrey-bion@github)
  * Reported #2265: Inconsistent handling of Collections$UnmodifiableList vs 
    Collections$UnmodifiableRandomAccessList
   (2.9.9)

Christoph Fiehe (cfiehe@github.com)
  * Contributed #2299: Fix for using jackson-databind in an OSGi environment under Android
   (2.9.9)

Cyril Martin (mcoolive@github.com)
  * Reported #2303: Deserialize null, when java type is "TypeRef of TypeRef of T",
    does not provide "Type(Type(null))"
   (2.9.9)

Daniil Barvitsky (dbarvitsky@github)
  * Reported #2324: `StringCollectionDeserializer` fails with custom collection
   (2.9.9)

Edgar Asatryan (nstdio@github)
  * Reported #2374: `ObjectMapper. getRegisteredModuleIds()` throws NPE if no modules registered
   (2.9.9.1)

Michael Simons (michael-simons@github)
  * Reported #2395: `NullPointerException` from `ResolvedRecursiveType` (regression due to
    fix for #2331)
   (2.9.9.3)

Joe Barnett (josephlbarnett@github)
  * Reported, contributed fix for #2404: FAIL_ON_MISSING_EXTERNAL_TYPE_ID_PROPERTY setting
    ignored when creator properties are buffered
   (2.9.10)

Kaki King (kingkk9279@g)
  * Reported #2449: Block one more gadget type (cve CVE-2019-14540)
   (2.9.10)

Jon Anderson (Jon901@github)
  * Reported #2544: java.lang.NoClassDefFoundError Thrown for compact profile1
   (2.9.10.2)

Zihui Ren (renzihui@github)
  * Suggested #2129: Add `SerializationFeature.WRITE_ENUM_KEYS_USING_INDEX`, separate from value setting
   (2.10.0)

Yiqiu Huang (huangyq23@github
  * Reported #2164: `FactoryBasedEnumDeserializer` does not respect
   `DeserializationFeature.WRAP_EXCEPTIONS`
   (2.10.0)
   
Alexander Saites (saites@github)
  * Reported #2189: `TreeTraversingParser` does not check int bounds
   (2.10.0)

Christoph Breitkopf (bokesan@github)
  * Reported #2217: Suboptimal memory allocation in `TextNode.getBinaryValue()`
   (2.10.0)

Pavel Chervakov (pacher@github)
  * Reported #2230: `WRITE_BIGDECIMAL_AS_PLAIN` is ignored if `@JsonFormat` is used
   (2.10.0)

Ben Anderson (andersonbd1@github)
  * Reported, suggested fix for #2309: READ_ENUMS_USING_TO_STRING doesn't support null values
   (2.10.0)

Manuel Hegner (manuel-hegner@github)
  * Suggested #2311: Unnecessary MultiView creation for property writers
   (2.10.0)

Chris Mercer (cmercer@github)
  * Reported #2331: `JsonMappingException` through nested getter with generic wildcard return type
   (2.10.0)

Robert Greig (rgreig@github)
  * Reported #2336: `MapDeserializer` can not merge `Map`s with polymorphic values
   (2.10.0)

Victor Noël (victornoel@github)
  * Reported #2338: Suboptimal return type for `JsonNode.withArray()`
   (2.10.0)
  * Reported #2339: Suboptimal return type for `ObjectNode.set()`
   (2.10.0)

David Harris (toadzky@github)
  * Reported #2378: `@JsonAlias` doesn't work with AutoValue
   (2.10.0)

Sam Smith (Oracle Security Researcher)
  * Suggested #2398: Replace recursion in `TokenBuffer.copyCurrentStructure()` with iteration

Vladimir Tsanev (tsachev@github)
  * Contributed #2415: Builder-based POJO deserializer should pass builder instance, not type,
    to `handleUnknownVanilla()` to fix earlier #822
   (2.10.0)

Marcos Passos (marcospassos@github(
  * Contributed #2432: Add support for module bundles
   (2.10.0)
  * Reported #2795: Cannot detect creator arguments of mixins for JDK types
   (2.11.3)

David Becker (dsbecker@github)
  * Suggested #2433: Improve `NullNode.equals()`
   (2.10.0)

Hesham Massoud (heshamMassoud@github)
  * Reported, contributed fix for #2442: `ArrayNode.addAll()` adds raw `null` values
    which cause NPE on `deepCopy()`
   (2.10.0)

David Connelly (dconnelly@github)
  * Reported #2446: Java 11: Unable to load JDK7 types (annotations, java.nio.file.Path):
    no Java7 support added
   (2.10.0)

Wahey (KevynBct@github)
  * Reported #2466: Didn't find class "java.nio.file.Path" below Android api 26
  (2.10.0)

Martín Coll (colltoaction@github)
  * Contributed #2467: Accept `JsonTypeInfo.As.WRAPPER_ARRAY` with no second argument to
   deserialize as "null value"
  (2.10.0)

Andrey Kulikov (ankulikov@github)
  * Reported #2457: Extended enum values are not handled as enums when used as Map keys
  (2.10.1)

João Guerra (joca-bt@github)
  * Reported #2473: Array index missing in path of `JsonMappingException` for `Collection<String>`,
    with custom deserializer
  (2.10.1)
  * Reported #2567: Incorrect target type for arrays when providing nulls and nulls are disabled
  (2.10.2)
  * Reported #2635: JsonParser cannot getText() for input stream on MismatchedInputException
  (2.11.0)
  * Reported #2770: JsonParser from MismatchedInputException cannot getText() for
    floating-point value
  (2.11.1)

Ryan Bohn (bohnman@github)
  * Reported #2475: `StringCollectionSerializer` calls `JsonGenerator.setCurrentValue(value)`,
    which messes up current value for sibling properties
  (2.10.1)

Johan Haleby (johanhaleby@github)
  * Reported #2513: BigDecimalAsStringSerializer in NumberSerializer throws IllegalStateException
    in 2.10
  (2.10.1)

Richard Wise (Woodz@github)
  * Reported #2519: Serializing `BigDecimal` values inside containers ignores shape override	
  (2.10.1)

Mark Schäfer (mark--@github)
  * Reported #2520: Sub-optimal exception message when failing to deserialize non-static inner classes
  (2.10.1)

Ruud Welling (WellingR@github)
  * Contributed fix for #2102: `FAIL_ON_NULL_FOR_PRIMITIVES` failure does not indicate
    field name in exception message
  (2.10.2)

Fabian Lange (CodingFabian@github)
  * Reported #2556: Contention in `TypeNameIdResolver.idFromClass()`
  (2.10.2)

Stefan Wendt (stewe@github)
  * Reported #2560: Check `WRAP_EXCEPTIONS` in `CollectionDeserializer.handleNonArray()`
  (2.10.2)

Greg Arakelian (arakelian@github)
  * Reported #2566: `MissingNode.toString()` returns `null` (4 character token) instead
    of empty string
  (2.10.2)

Kamal Aslam (aslamkam@github)
  * Reported #2482: `JSONMappingException` `Location` column number is one line
    Behind the actual location
  (2.10.3)

Tobias Preuss (johnjohndoe@github)
  * Reported #2599: NoClassDefFoundError at DeserializationContext.<init> on Android 4.1.2
    and Jackson 2.10.0
  (2.10.3)

Eduard Tudenhöfner (nastra@github)
  * Reported #2602, contributed fix for: ByteBufferSerializer produces unexpected results with
    a duplicated ByteBuffer and a position > 0
  (2.10.3)

Alexander Shilov (ashlanderr@github)
  * Reported, suggested fix for #2610: `EXTERNAL_PROPERTY` doesn't work with `@JsonIgnoreProperties`
  (2.10.3)

Endre Stølsvik (stolsvik@github)
  * Reported #2679: `ObjectMapper.readValue("123", Void.TYPE)` throws "should never occur"
  (2.10.4)

Denis Kostousov (kostousov-ds@github)
  * Reported #2787 (partial fix): NPE after add mixin for enum
  (2.10.5)

Máté Rédecsi (rmatesz@github)
  * Reported #953: i-I case convertion problem in Turkish locale with case-insensitive deserialization
  (2.11.0)

Ville Koskela (vjkoskela@github)
  * Contributed #2487: BeanDeserializerBuilder Protected Factory Method for Extension
  (2.11.0)
  * Reported #2486: Builder Deserialization with JsonCreator Value vs Array
  (2.11.1)
  * Contributed fix for #792: Deserialization Not Working Right with Generic Types and Builders
  (2.12.0)

Fitz (Joongsoo.Park) (joongsoo@github)
  * Contributed #2511: Add `SerializationFeature.WRITE_SELF_REFERENCES_AS_NULL`
  (2.11.0)

Antonio Petrelli (apetrelli@github)
  * Reported #2049: TreeTraversingParser and UTF8StreamJsonParser create contexts differently
  (2.11.0)

Robert Diebels (RobertDiebels@github)
  * Contributed #2352: Support use of `@JsonAlias` for enum values
  (2.11.0)

Joseph Koshakow (jkosh44@github)
  * Contributed fix for #2515: `ObjectMapper.registerSubtypes(NamedType...)` doesn't allow registering
    the same POJO for two different type ids
  (2.11.0)

Haowei Wen (yushijinhun@github)
  * Reported #2565: Java 8 `Optional` not working with `@JsonUnwrapped` on unwrappable type
  (2.11.0)

Bartosz Baranowski (baranowb@github)
  * Reported #2589: `DOMDeserializer`: setExpandEntityReferences(false) may not prevent
   external entity expansion in all cases
  (2.11.0)

Oleksii Khomchenko (gagoman@github)
  * Reported, contributed fix for #2592: `ObjectMapper.setSerializationInclusion()` is
   ignored for `JsonAnyGetter`
  (2.11.0)

Oleksandr Poslavskyi (alevskyi@github)
  * Contributed fix for #1983: Polymorphic deserialization should handle case-insensitive Type Id
    property name if `MapperFeature.ACCEPT_CASE_INSENSITIVE_PROPERTIES` is enabled
  (2.11.0)

Simone D'Avico (simonedavico@github)
  * Reported #2632: Failure to resolve generic type parameters on serialization
  (2.11.0)
 
Robin Roos (robinroos@github)
  * Contributed #2636: ObjectReader readValue lacks Class<T> argument
  (2.11.0)

Michael Cramer (BigMichi1@github)
  * Reported #2725: JsonCreator on static method in Enum and Enum used as key in map
   fails randomly
  (2.11.1)
 
Frank Schmager (fschmager@github)
  * Reported #2757: "Conflicting setter definitions for property" exception for `Map`
   subtype during deserialization
  (2.11.1)

Johannes Kuhn (DasBrain@github)
  * Reported #2758: Fail to deserialize local Records
  (2.11.1)
  * Reported #2760: Jackson doesn't respect `CAN_OVERRIDE_ACCESS_MODIFIERS=false` for
    deserializer properties
  (2.11.1)

Oleg Chtchoukine (oshatrk@github)
  * Reported #2759: Rearranging of props when property-based generator is in use leads
    to incorrect output
  (2.11.1)

Joshua Shannon (retrodaredevil@github)
  * Reported, contributed fix for #2785: Polymorphic subtypes not registering on copied
    ObjectMapper (2.11.1)
  (2.11.2)

Daniel Hrabovcak (TheSpiritXIII@github)
  * Reported #2796: `TypeFactory.constructType()` does not take `TypeBindings` correctly
  (2.11.2)

Lari Hotari (lhotari@github)
  * Reported #2821: Json serialization fails or a specific case that contains generics and
   static methods with generic parameters (2.11.1 -> 2.11.2 regression)
  (2.11.3)

Nils Christian Ehmke (nils-christian@github)
  * Reported #2822: Using JsonValue and JsonFormat on one field does not work as expected
  (2.11.3)

Daniel Wu (DanielYWoo@github)
  * Reported #2840: `ObjectMapper.activateDefaultTypingAsProperty()` is not using
  (2.11.3)

Marc Carter (drekbour@github)
  * Contributed #43 implementation: Add option to resolve type from multiple existing properties,
   `@JsonTypeInfo(use=DEDUCTION)`
  (2.12.0)
 
Mike Gilbode (gilbode@github)
  * Reported #792: Deserialization Not Working Right with Generic Types and Builders
  (2.12.0)

Baptiste Pernet (sp4ce@github)
  * Contributed #1296 implementation: Add `@JsonIncludeProperties(propertyNames)` (reverse
    of `@JsonIgnoreProperties`)
  (2.12.0)

Patrick Jungermann (pjungermann@github)
  * Requested #1852: Allow case insensitive deserialization of String value into
  `boolean`/`Boolean` (esp for Excel)
  (2.12.0)
 
Nate Bauernfeind (nbauernfeind@github)
  * Reported #2091: `ReferenceType` does not expose valid containedType
  (2.12.0)

Xiang Zhang (zhangyangyu@github)
  * Reported #2118: `JsonProperty.Access.READ_ONLY` does not work with "getter-as-setter"
    Collections
  (2.12.0)

David Nelson (eatdrinksleepcode@github)
  * Requested #2215: Support `BigInteger` and `BigDecimal` creators in `StdValueInstantiator`
  (2.12.0)

Tiago Martins (upsidedownsmile@github)
  * Contributed #2215: Support `BigInteger` and `BigDecimal` creators in `StdValueInstantiator`
  (2.12.0)

Yona Appletree (Yona-Appletree@github)
  * Reported #2283: `JsonProperty.Access.READ_ONLY` fails with collections when a
    property name is specified
  (2.12.0)

Youri Bonnaffé (youribonnaffe@github)
  * Contributed #2709: Support for JDK 14 record types
  (2.12.0)

David Bidorff (bidorffOL@github)
  * Reported, contributed fix for #2719: `FAIL_ON_IGNORED_PROPERTIES` does not throw
    on `READONLY` properties with an explicit name
  (2.12.0)

Jendrik Johannes (jjohannes@github)
  * Contributed #2726: Add Gradle Module Metadata for version alignment with Gradle 6
  (2.12.0)

Swayam Raina (swayamraina@github)
  * Contributed #2761: Support multiple names in `JsonSubType.Type`
  (2.12.0)

Ilya Golovin (ilgo0413@github)
  * Contributed #2873: `MapperFeature.ACCEPT_CASE_INSENSITIVE_ENUMS` should work for enum as keys
  (2.12.0)
