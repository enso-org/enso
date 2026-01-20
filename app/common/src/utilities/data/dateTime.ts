/** @file Utilities for manipulating and displaying dates and times. */
import { ZonedDateTime, getDayOfWeek } from '@internationalized/date'
import type { TextId } from '../../text.js'
import { newtypeConstructor, type Newtype } from './newtype.js'

// 0 = Monday. Use `en-US` for 0 = Sunday.
const DAY_OF_WEEK_LOCALE = 'en-GB'
const ISO_FORMAT = Intl.DateTimeFormat('sv', { dateStyle: 'short', timeStyle: 'short' })
/** The number of hours in half a day. This is used to get the number of hours for AM/PM time. */
export const HALF_DAY_HOURS = 12
export const MAX_DAYS_PER_MONTH = 31
export const DAYS_PER_WEEK = 7
export const MONTHS_PER_YEAR = 12
export const MINUTE_MS = 60_000
/** The number of minutes in an hour. */
export const HOUR_MINUTES = 60

/** All possible day numbers. */
export const DAYS = [...Array(DAYS_PER_WEEK).keys()] as const
/** All possible month numbers. */
export const MONTHS = [...Array(MONTHS_PER_YEAR).keys()] as const

/** A mapping from the month index returned by {@link Date.getMonth} to its full name. */
export const MONTH_NAMES = [
  'January',
  'February',
  'March',
  'April',
  'May',
  'June',
  'July',
  'August',
  'September',
  'October',
  'November',
  'December',
]

export const DAY_3_LETTER_TEXT_IDS = [
  'monday3',
  'tuesday3',
  'wednesday3',
  'thursday3',
  'friday3',
  'saturday3',
  'sunday3',
] satisfies TextId[]

export const DAY_TEXT_IDS = [
  'monday',
  'tuesday',
  'wednesday',
  'thursday',
  'friday',
  'saturday',
  'sunday',
] satisfies TextId[]

export const MONTH_3_LETTER_TEXT_IDS = [
  'january3',
  'february3',
  'march3',
  'april3',
  'may3',
  'june3',
  'july3',
  'august3',
  'september3',
  'october3',
  'november3',
  'december3',
] satisfies TextId[]

export const WHITELISTED_TIME_ZONE_INFO = [
  { timeZone: 'Etc/GMT+12', description: 'International Date Line West' },
  { timeZone: 'Etc/GMT+11', description: 'Coordinated Universal Time-11' },
  { timeZone: 'Pacific/Midway', description: 'Midway' },
  { timeZone: 'Pacific/Niue', description: 'Niue' },
  { timeZone: 'Pacific/Pago_Pago', description: 'Pago_Pago' },
  { timeZone: 'Pacific/Samoa', description: 'Samoa' },
  { timeZone: 'America/Adak', description: 'Adak' },
  { timeZone: 'Pacific/Rarotonga', description: 'Rarotonga' },
  { timeZone: 'Pacific/Tahiti', description: 'Tahiti' },
  { timeZone: 'Pacific/Honolulu', description: 'Hawaii' },
  { timeZone: 'Pacific/Marquesas', description: 'Marquesas Islands' },
  { timeZone: 'America/Anchorage', description: 'Alaska' },
  { timeZone: 'Pacific/Gambier', description: 'Gambier' },
  { timeZone: 'America/Juneau', description: 'Juneau' },
  { timeZone: 'America/Nome', description: 'Nome' },
  { timeZone: 'America/Metlakatla', description: 'Metlakatla' },
  { timeZone: 'America/Sitka', description: 'Sitka' },
  { timeZone: 'America/Yakutat', description: 'Yakutat' },
  { timeZone: 'America/Tijuana', description: 'Baja California' },
  { timeZone: 'America/Los_Angeles', description: 'Pacific Time (US & Canada)' },
  { timeZone: 'Pacific/Pitcairn', description: 'Pitcairn' },
  { timeZone: 'America/Creston', description: 'Creston' },
  { timeZone: 'America/Chihuahua', description: 'Chihuahua, Mazatlan' },
  { timeZone: 'America/Dawson', description: 'Dawson' },
  { timeZone: 'America/Dawson_Creek', description: 'Dawson Creek' },
  { timeZone: 'America/Denver', description: 'Mountain Time (US & Canada)' },
  { timeZone: 'America/Ensenada', description: 'Ensenada' },
  { timeZone: 'America/Fort_Nelson', description: 'Fort Nelson' },
  { timeZone: 'America/Hermosillo', description: 'Hermosillo' },
  { timeZone: 'America/Vancouver', description: 'Vancouver' },
  { timeZone: 'America/Whitehorse', description: 'Whitehorse' },
  { timeZone: 'America/Phoenix', description: 'Arizona' },
  { timeZone: 'America/Bahia_Banderas', description: 'Bahia Banderas' },
  { timeZone: 'America/Belize', description: 'Belize' },
  { timeZone: 'America/Boise', description: 'Boise' },
  { timeZone: 'America/Costa_Rica', description: 'Costa Rica' },
  { timeZone: 'America/Chicago', description: 'Central Time (US & Canada)' },
  { timeZone: 'America/Edmonton', description: 'Edmonton' },
  { timeZone: 'America/El_Salvador', description: 'El Salvador' },
  { timeZone: 'America/Guatemala', description: 'Central America' },
  { timeZone: 'America/Managua', description: 'Managua' },
  { timeZone: 'America/Mazatlan', description: 'Mazatlan' },
  { timeZone: 'America/Ojinaga', description: 'Ojinaga' },
  { timeZone: 'America/Swift_Current', description: 'Swift Current' },
  { timeZone: 'America/Tegucigalpa', description: 'Tegucigalpa' },
  { timeZone: 'America/Mexico_City', description: 'Guadalajara, Mexico City, Monterrey' },
  { timeZone: 'America/Regina', description: 'Saskatchewan' },
  { timeZone: 'Pacific/Galapagos', description: 'Galapagos' },
  { timeZone: 'America/Atikokan', description: 'Atikokan' },
  { timeZone: 'America/Bogota', description: 'Bogota, Lima, Quito' },
  { timeZone: 'America/Cancun', description: 'Cancun' },
  { timeZone: 'America/Cayman', description: 'Cayman' },
  { timeZone: 'America/Coral_Harbour', description: 'Coral Harbour' },
  { timeZone: 'America/Eirunepe', description: 'Eirunepe' },
  { timeZone: 'America/Guayaquil', description: 'Guayaquil' },
  { timeZone: 'America/Indianapolis', description: 'Indiana (East)' },
  { timeZone: 'America/Jamaica', description: 'Jamaica' },
  { timeZone: 'America/Lima', description: 'Lima' },
  { timeZone: 'America/Matamoros', description: 'Matamoros' },
  { timeZone: 'America/Menominee', description: 'Menominee' },
  { timeZone: 'America/Merida', description: 'Merida' },
  { timeZone: 'America/Monterrey', description: 'Monterrey' },
  { timeZone: 'America/Nipigon', description: 'Nipigon' },
  { timeZone: 'America/Panama', description: 'Panama' },
  { timeZone: 'America/Rainy_River', description: 'Rainy River' },
  { timeZone: 'America/Rio_Branco', description: 'Rio Branco' },
  { timeZone: 'America/Thunder_Bay', description: 'Thunder Bay' },
  { timeZone: 'America/Winnipeg', description: 'Winnipeg' },
  { timeZone: 'America/New_York', description: 'Eastern Time (US & Canada)' },
  { timeZone: 'Pacific/Easter', description: 'Easter' },
  { timeZone: 'America/Caracas', description: 'Caracas' },
  { timeZone: 'America/Anguilla', description: 'Anguilla' },
  { timeZone: 'America/Antigua', description: 'Antigua' },
  { timeZone: 'America/Aruba', description: 'Aruba' },
  { timeZone: 'America/Asuncion', description: 'Asuncion' },
  { timeZone: 'America/Barbados', description: 'Barbados' },
  { timeZone: 'America/Boa_Vista', description: 'Boa Vista' },
  { timeZone: 'America/Campo_Grande', description: 'Campo Grande' },
  { timeZone: 'America/Curacao', description: 'Curacao' },
  { timeZone: 'America/Cuiaba', description: 'Cuiaba' },
  { timeZone: 'America/Detroit', description: 'Detroit' },
  { timeZone: 'America/Dominica', description: 'Dominica' },
  { timeZone: 'America/Grand_Turk', description: 'Grand Turk' },
  { timeZone: 'America/Grenada', description: 'Grenada' },
  { timeZone: 'America/Guadeloupe', description: 'Guadeloupe' },
  { timeZone: 'America/Guyana', description: 'Guyana' },
  { timeZone: 'America/Halifax', description: 'Atlantic Time (Canada)' },
  { timeZone: 'America/Havana', description: 'Havana' },
  { timeZone: 'America/La_Paz', description: 'Georgetown, La Paz, Manaus, San Juan' },
  { timeZone: 'America/Manaus', description: 'Manaus' },
  { timeZone: 'America/Martinique', description: 'Martinique' },
  { timeZone: 'America/Montreal', description: 'Montreal' },
  { timeZone: 'America/Montserrat', description: 'Montserrat' },
  { timeZone: 'America/Nassau', description: 'Nassau' },
  { timeZone: 'America/Port_of_Spain', description: 'Port of Spain' },
  { timeZone: 'America/Porto_Velho', description: 'Porto Velho' },
  { timeZone: 'America/Puerto_Rico', description: 'Puerto Rico' },
  { timeZone: 'America/Santo_Domingo', description: 'Santo Domingo' },
  { timeZone: 'America/St_Kitts', description: 'St. Kitts' },
  { timeZone: 'America/St_Lucia', description: 'St. Lucia' },
  { timeZone: 'America/St_Thomas', description: 'St. Thomas' },
  { timeZone: 'America/St_Vincent', description: 'St. Vincent' },
  { timeZone: 'America/Toronto', description: 'Toronto' },
  { timeZone: 'America/Tortola', description: 'Tortola' },
  { timeZone: 'America/Santiago', description: 'Santiago' },
  { timeZone: 'America/St_Johns', description: 'Newfoundland' },
  { timeZone: 'America/Araguaina', description: 'Araguaina' },
  { timeZone: 'America/Bahia', description: 'Bahia' },
  { timeZone: 'America/Belem', description: 'Belem' },
  { timeZone: 'America/Buenos_Aires', description: 'Buenos Aires' },
  { timeZone: 'America/Cayenne', description: 'Cayenne, Fortaleza' },
  { timeZone: 'America/Fortaleza', description: 'Fortaleza' },
  { timeZone: 'America/Glace_Bay', description: 'Glace Bay' },
  { timeZone: 'America/Goose_Bay', description: 'Goose Bay' },
  { timeZone: 'America/Godthab', description: 'Greenland' },
  { timeZone: 'America/Maceio', description: 'Maceio' },
  { timeZone: 'America/Moncton', description: 'Moncton' },
  { timeZone: 'America/Paramaribo', description: 'Paramaribo' },
  { timeZone: 'America/Punta_Arenas', description: 'Punta Arenas' },
  { timeZone: 'America/Recife', description: 'Recife' },
  { timeZone: 'America/Rosario', description: 'Rosario' },
  { timeZone: 'America/Santarem', description: 'Santarem' },
  { timeZone: 'America/Thule', description: 'Thule' },
  { timeZone: 'America/Montevideo', description: 'Montevideo' },
  { timeZone: 'America/Sao_Paulo', description: 'Brasilia' },
  { timeZone: 'Antarctica/Rothera', description: 'Rothera' },
  { timeZone: 'Atlantic/Bermuda', description: 'Bermuda' },
  { timeZone: 'Atlantic/Stanley', description: 'Stanley' },
  { timeZone: 'America/Miquelon', description: 'Miquelon' },
  { timeZone: 'America/Noronha', description: 'Noronha' },
  { timeZone: 'Atlantic/South_Georgia', description: 'South Georgia' },
  { timeZone: 'Etc/GMT+2', description: 'Coordinated Universal Time-02' },
  { timeZone: 'Atlantic/Azores', description: 'Azores' },
  { timeZone: 'Atlantic/Cape_Verde', description: 'Cape Verde' },
  { timeZone: 'UTC', description: 'UTC' },
  { timeZone: 'America/Danmarkshavn', description: 'Danmarkshavn' },
  { timeZone: 'America/Scoresbysund', description: 'Scoresbysund' },
  { timeZone: 'Africa/Accra', description: 'Accra' },
  { timeZone: 'Africa/Abidjan', description: 'Abidjan' },
  { timeZone: 'Africa/Bamako', description: 'Bamako' },
  { timeZone: 'Africa/Banjul', description: 'Banjul' },
  { timeZone: 'Africa/Bissau', description: 'Bissau' },
  { timeZone: 'Africa/Conakry', description: 'Conakry' },
  { timeZone: 'Africa/Dakar', description: 'Dakar' },
  { timeZone: 'Africa/El_Aaiun', description: 'El Aaiun' },
  { timeZone: 'Africa/Freetown', description: 'Freetown' },
  { timeZone: 'Africa/Lome', description: 'Lome' },
  { timeZone: 'Africa/Monrovia', description: 'Monrovia' },
  { timeZone: 'Africa/Nouakchott', description: 'Nouakchott' },
  { timeZone: 'Africa/Ouagadougou', description: 'Ouagadougou' },
  { timeZone: 'Africa/Sao_Tome', description: 'Sao Tome' },
  { timeZone: 'Africa/Timbuktu', description: 'Timbuktu' },
  { timeZone: 'Antarctica/Troll', description: 'Troll' },
  { timeZone: 'Atlantic/St_Helena', description: 'St. Helena' },
  { timeZone: 'Atlantic/Reykjavik', description: 'Monrovia, Reykjavik' },
  {
    timeZone: 'Europe/London',
    description: 'Greenwich Mean Time, Dublin, Edinburgh, Lisbon, London',
  },
  { timeZone: 'Africa/Bangui', description: 'Bangui' },
  { timeZone: 'Africa/Casablanca', description: 'Casablanca' },
  { timeZone: 'Africa/Algiers', description: 'Algiers' },
  { timeZone: 'Africa/Brazzaville', description: 'Brazzaville' },
  { timeZone: 'Africa/Douala', description: 'Douala' },
  { timeZone: 'Africa/Kinshasa', description: 'Kinshasa' },
  { timeZone: 'Africa/Libreville', description: 'Libreville' },
  { timeZone: 'Africa/Luanda', description: 'Luanda' },
  { timeZone: 'Africa/Malabo', description: 'Malabo' },
  { timeZone: 'Africa/Ndjamena', description: 'Ndjamena' },
  { timeZone: 'Africa/Niamey', description: 'Niamey' },
  { timeZone: 'Africa/Tunis', description: 'Tunis' },
  { timeZone: 'Africa/Lagos', description: 'West Central Africa' },
  { timeZone: 'Atlantic/Canary', description: 'Canary' },
  { timeZone: 'Atlantic/Faroe', description: 'Faroe' },
  { timeZone: 'Atlantic/Madeira', description: 'Madeira' },
  { timeZone: 'Europe/Belfast', description: 'Belfast' },
  { timeZone: 'Europe/Berlin', description: 'Amsterdam, Berlin, Bern, Rome, Stockholm, Vienna' },
  { timeZone: 'Europe/Budapest', description: 'Belgrade, Bratislava, Budapest, Ljubljana, Prague' },
  { timeZone: 'Europe/Dublin', description: 'Dublin' },
  { timeZone: 'Europe/Guernsey', description: 'Guernsey' },
  { timeZone: 'Europe/Isle_of_Man', description: 'Isle of Man' },
  { timeZone: 'Europe/Jersey', description: 'Jersey' },
  { timeZone: 'Europe/Lisbon', description: 'Lisbon' },
  { timeZone: 'Europe/Paris', description: 'Brussels, Copenhagen, Madrid, Paris' },
  { timeZone: 'Europe/Warsaw', description: 'Sarajevo, Skopje, Warsaw, Zagreb' },
  { timeZone: 'Africa/Blantyre', description: 'Blantyre' },
  { timeZone: 'Africa/Bujumbura', description: 'Bujumbura' },
  { timeZone: 'Africa/Cairo', description: 'Cairo' },
  { timeZone: 'Africa/Ceuta', description: 'Ceuta' },
  { timeZone: 'Africa/Johannesburg', description: 'Johannesburg' },
  { timeZone: 'Africa/Gaborone', description: 'Gaborone' },
  { timeZone: 'Africa/Harare', description: 'Harare' },
  { timeZone: 'Africa/Khartoum', description: 'Khartoum' },
  { timeZone: 'Africa/Kigali', description: 'Kigali' },
  { timeZone: 'Africa/Lubumbashi', description: 'Lubumbashi' },
  { timeZone: 'Africa/Lusaka', description: 'Lusaka' },
  { timeZone: 'Africa/Maputo', description: 'Maputo' },
  { timeZone: 'Africa/Maseru', description: 'Maseru' },
  { timeZone: 'Africa/Mbabane', description: 'Mbabane' },
  { timeZone: 'Africa/Tripoli', description: 'Tripoli' },
  { timeZone: 'Africa/Windhoek', description: 'Windhoek' },
  { timeZone: 'Asia/Amman', description: 'Amman' },
  { timeZone: 'Asia/Beirut', description: 'Beirut' },
  { timeZone: 'Asia/Damascus', description: 'Damascus' },
  { timeZone: 'Asia/Jerusalem', description: 'Jerusalem' },
  { timeZone: 'Europe/Amsterdam', description: 'Amsterdam' },
  { timeZone: 'Europe/Andorra', description: 'Andorra' },
  { timeZone: 'Europe/Belgrade', description: 'Belgrade' },
  { timeZone: 'Europe/Brussels', description: 'Brussels' },
  { timeZone: 'Europe/Copenhagen', description: 'Copenhagen' },
  { timeZone: 'Europe/Gibraltar', description: 'Gibraltar' },
  { timeZone: 'Europe/Istanbul', description: 'Athens, Bucharest, Istanbul' },
  { timeZone: 'Europe/Kaliningrad', description: 'Kaliningrad' },
  { timeZone: 'Europe/Kiev', description: 'Helsinki, Kyiv, Riga, Sofia, Tallinn, Vilnius' },
  { timeZone: 'Europe/Ljubljana', description: 'Ljubljana' },
  { timeZone: 'Europe/Luxembourg', description: 'Luxembourg' },
  { timeZone: 'Europe/Madrid', description: 'Madrid' },
  { timeZone: 'Europe/Malta', description: 'Malta' },
  { timeZone: 'Europe/Monaco', description: 'Monaco' },
  { timeZone: 'Europe/Minsk', description: 'Minsk' },
  { timeZone: 'Europe/Oslo', description: 'Oslo' },
  { timeZone: 'Europe/Prague', description: 'Prague' },
  { timeZone: 'Europe/Rome', description: 'Rome' },
  { timeZone: 'Europe/Sarajevo', description: 'Sarajevo' },
  { timeZone: 'Europe/Skopje', description: 'Skopje' },
  { timeZone: 'Europe/Stockholm', description: 'Stockholm' },
  { timeZone: 'Europe/Tirane', description: 'Tirane' },
  { timeZone: 'Europe/Vaduz', description: 'Vaduz' },
  { timeZone: 'Europe/Vienna', description: 'Vienna' },
  { timeZone: 'Europe/Zagreb', description: 'Zagreb' },
  { timeZone: 'Europe/Zurich', description: 'Zurich' },
  { timeZone: 'Africa/Addis_Ababa', description: 'Addis Ababa' },
  { timeZone: 'Africa/Asmara', description: 'Asmara' },
  { timeZone: 'Africa/Dar_es_Salaam', description: 'Dar es Salaam' },
  { timeZone: 'Africa/Djibouti', description: 'Djibouti' },
  { timeZone: 'Africa/Juba', description: 'Juba' },
  { timeZone: 'Africa/Kampala', description: 'Kampala' },
  { timeZone: 'Africa/Mogadishu', description: 'Mogadishu' },
  { timeZone: 'Africa/Nairobi', description: 'Nairobi' },
  { timeZone: 'Asia/Aden', description: 'Aden' },
  { timeZone: 'Asia/Bahrain', description: 'Bahrain' },
  { timeZone: 'Asia/Baghdad', description: 'Baghdad' },
  { timeZone: 'Asia/Famagusta', description: 'Famagusta' },
  { timeZone: 'Asia/Gaza', description: 'Gaza' },
  { timeZone: 'Asia/Hebron', description: 'Hebron' },
  { timeZone: 'Asia/Kuwait', description: 'Kuwait' },
  { timeZone: 'Asia/Nicosia', description: 'Nicosia' },
  { timeZone: 'Asia/Qatar', description: 'Qatar' },
  { timeZone: 'Asia/Tel_Aviv', description: 'Tel Aviv' },
  { timeZone: 'Asia/Riyadh', description: 'Kuwait, Riyadh' },
  { timeZone: 'Europe/Athens', description: 'Athens' },
  { timeZone: 'Europe/Bucharest', description: 'Bucharest' },
  { timeZone: 'Europe/Chisinau', description: 'Chisinau' },
  { timeZone: 'Europe/Helsinki', description: 'Helsinki' },
  { timeZone: 'Europe/Kirov', description: 'Kirov' },
  { timeZone: 'Europe/Moscow', description: 'Moscow, St. Petersburg, Volgograd' },
  { timeZone: 'Europe/Riga', description: 'Riga' },
  { timeZone: 'Europe/Simferopol', description: 'Simferopol' },
  { timeZone: 'Europe/Sofia', description: 'Sofia' },
  { timeZone: 'Europe/Tallinn', description: 'Tallinn' },
  { timeZone: 'Europe/Tiraspol', description: 'Tiraspol' },
  { timeZone: 'Europe/Uzhgorod', description: 'Uzhgorod' },
  { timeZone: 'Europe/Vilnius', description: 'Vilnius' },
  { timeZone: 'Europe/Volgograd', description: 'Volgograd' },
  { timeZone: 'Europe/Zaporozhye', description: 'Zaporozhye' },
  { timeZone: 'Indian/Antananarivo', description: 'Antananarivo' },
  { timeZone: 'Indian/Comoro', description: 'Comoro' },
  { timeZone: 'Indian/Mayotte', description: 'Mayotte' },
  { timeZone: 'Asia/Tehran', description: 'Tehran' },
  { timeZone: 'Asia/Baku', description: 'Baku' },
  { timeZone: 'Asia/Muscat', description: 'Muscat' },
  { timeZone: 'Asia/Dubai', description: 'Abu Dhabi, Muscat' },
  { timeZone: 'Asia/Tbilisi', description: 'Tbilisi' },
  { timeZone: 'Asia/Yerevan', description: 'Yerevan' },
  { timeZone: 'Europe/Astrakhan', description: 'Astrakhan' },
  { timeZone: 'Europe/Samara', description: 'Samara' },
  { timeZone: 'Europe/Saratov', description: 'Saratov' },
  { timeZone: 'Europe/Ulyanovsk', description: 'Ulyanovsk' },
  { timeZone: 'Indian/Mahe', description: 'Mahe' },
  { timeZone: 'Indian/Mauritius', description: 'Port Louis' },
  { timeZone: 'Indian/Reunion', description: 'Reunion' },
  { timeZone: 'Asia/Kabul', description: 'Kabul' },
  { timeZone: 'Antarctica/Mawson', description: 'Mawson' },
  { timeZone: 'Asia/Aqtau', description: 'Aqtau' },
  { timeZone: 'Asia/Aqtobe', description: 'Aqtobe' },
  { timeZone: 'Asia/Ashgabat', description: 'Ashgabat' },
  { timeZone: 'Asia/Atyrau', description: 'Atyrau' },
  { timeZone: 'Asia/Dushanbe', description: 'Dushanbe' },
  { timeZone: 'Asia/Oral', description: 'Oral' },
  { timeZone: 'Asia/Samarkand', description: 'Samarkand' },
  { timeZone: 'Asia/Karachi', description: 'Islamabad, Karachi' },
  { timeZone: 'Asia/Tashkent', description: 'Tashkent' },
  { timeZone: 'Asia/Yekaterinburg', description: 'Yekaterinburg' },
  { timeZone: 'Indian/Maldives', description: 'Maldives' },
  { timeZone: 'Asia/Calcutta', description: 'Chennai, Kolkata, Mumbai, New Delhi' },
  { timeZone: 'Asia/Colombo', description: 'Sri Jayawardenepura' },
  { timeZone: 'Asia/Kolkata', description: 'Kolkata' },
  { timeZone: 'Asia/Kathmandu', description: 'Kathmandu' },
  { timeZone: 'Asia/Almaty', description: 'Astana' },
  { timeZone: 'Asia/Bishkek', description: 'Bishkek' },
  { timeZone: 'Asia/Kashgar', description: 'Kashgar' },
  { timeZone: 'Asia/Omsk', description: 'Omsk' },
  { timeZone: 'Asia/Qyzylorda', description: 'Qyzylorda' },
  { timeZone: 'Asia/Thimphu', description: 'Thimphu' },
  { timeZone: 'Asia/Urumqi', description: 'Urumqi' },
  { timeZone: 'Asia/Dhaka', description: 'Dhaka' },
  { timeZone: 'Asia/Novosibirsk', description: 'Novosibirsk' },
  { timeZone: 'Indian/Chagos', description: 'Chagos' },
  { timeZone: 'Asia/Yangon', description: 'Yangon' },
  { timeZone: 'Asia/Rangoon', description: 'Yangon (Rangoon)' },
  { timeZone: 'Indian/Cocos', description: 'Cocos' },
  { timeZone: 'Antarctica/Davis', description: 'Davis' },
  { timeZone: 'Asia/Barnaul', description: 'Barnaul' },
  { timeZone: 'Asia/Bangkok', description: 'Bangkok, Hanoi, Jakarta' },
  { timeZone: 'Asia/Ho_Chi_Minh', description: 'Ho Chi Minh' },
  { timeZone: 'Asia/Hovd', description: 'Hovd' },
  { timeZone: 'Asia/Jakarta', description: 'Jakarta' },
  { timeZone: 'Asia/Novokuznetsk', description: 'Novokuznetsk' },
  { timeZone: 'Asia/Phnom_Penh', description: 'Phnom Penh' },
  { timeZone: 'Asia/Pontianak', description: 'Pontianak' },
  { timeZone: 'Asia/Tomsk', description: 'Tomsk' },
  { timeZone: 'Asia/Vientiane', description: 'Vientiane' },
  { timeZone: 'Asia/Krasnoyarsk', description: 'Krasnoyarsk' },
  { timeZone: 'Indian/Christmas', description: 'Christmas' },
  { timeZone: 'Antarctica/Casey', description: 'Casey' },
  { timeZone: 'Asia/Brunei', description: 'Brunei' },
  { timeZone: 'Asia/Choibalsan', description: 'Choibalsan' },
  { timeZone: 'Asia/Chongqing', description: 'Chongqing' },
  { timeZone: 'Asia/Harbin', description: 'Harbin' },
  { timeZone: 'Asia/Hong_Kong', description: 'Hong Kong' },
  { timeZone: 'Asia/Kuala_Lumpur', description: 'Kuala Lumpur' },
  { timeZone: 'Asia/Kuching', description: 'Kuching' },
  { timeZone: 'Asia/Macau', description: 'Macau' },
  { timeZone: 'Asia/Makassar', description: 'Makassar' },
  { timeZone: 'Asia/Manila', description: 'Manila' },
  { timeZone: 'Asia/Irkutsk', description: 'Irkutsk' },
  { timeZone: 'Asia/Shanghai', description: 'Beijing, Chongqing, Hong Kong, Urumqi' },
  { timeZone: 'Asia/Singapore', description: 'Kuala Lumpur, Singapore' },
  { timeZone: 'Asia/Taipei', description: 'Taipei' },
  { timeZone: 'Asia/Ulaanbaatar', description: 'Ulaanbaatar' },
  { timeZone: 'Australia/Perth', description: 'Perth' },
  { timeZone: 'Asia/Pyongyang', description: 'Pyongyang' },
  { timeZone: 'Australia/Eucla', description: 'Eucla' },
  { timeZone: 'Asia/Chita', description: 'Chita' },
  { timeZone: 'Asia/Dili', description: 'Dili' },
  { timeZone: 'Asia/Jayapura', description: 'Jayapura' },
  { timeZone: 'Asia/Khandyga', description: 'Khandyga' },
  { timeZone: 'Asia/Tokyo', description: 'Osaka, Sapporo, Tokyo' },
  { timeZone: 'Asia/Seoul', description: 'Seoul' },
  { timeZone: 'Asia/Yakutsk', description: 'Yakutsk' },
  { timeZone: 'Pacific/Palau', description: 'Palau' },
  { timeZone: 'Australia/Adelaide', description: 'Adelaide' },
  { timeZone: 'Australia/Broken_Hill', description: 'Broken Hill' },
  { timeZone: 'Australia/Darwin', description: 'Darwin' },
  { timeZone: 'Australia/Brisbane', description: 'Brisbane' },
  { timeZone: 'Asia/Vladivostok', description: 'Vladivostok' },
  { timeZone: 'Australia/Hobart', description: 'Hobart' },
  { timeZone: 'Australia/Lindeman', description: 'Lindeman' },
  { timeZone: 'Pacific/Chuuk', description: 'Chuuk' },
  { timeZone: 'Pacific/Saipan', description: 'Saipan' },
  { timeZone: 'Pacific/Port_Moresby', description: 'Guam, Port Moresby' },
  { timeZone: 'Australia/Lord_Howe', description: 'Lord Howe Island' },
  { timeZone: 'Asia/Sakhalin', description: 'Sakhalin' },
  { timeZone: 'Asia/Srednekolymsk', description: 'Srednekolymsk' },
  { timeZone: 'Asia/Magadan', description: 'Magadan, Solomon Is., New Caledonia' },
  { timeZone: 'Australia/Currie', description: 'Currie' },
  { timeZone: 'Australia/Melbourne', description: 'Melbourne' },
  { timeZone: 'Australia/Sydney', description: 'Sydney' },
  { timeZone: 'Pacific/Efate', description: 'Efate' },
  { timeZone: 'Pacific/Bougainville', description: 'Bougainville' },
  { timeZone: 'Pacific/Guadalcanal', description: 'Guadalcanal' },
  { timeZone: 'Pacific/Kosrae', description: 'Kosrae' },
  { timeZone: 'Pacific/Norfolk', description: 'Norfolk' },
  { timeZone: 'Pacific/Noumea', description: 'Noumea' },
  { timeZone: 'Pacific/Pohnpei', description: 'Pohnpei' },
  { timeZone: 'Asia/Anadyr', description: 'Anadyr' },
  { timeZone: 'Asia/Kamchatka', description: 'Petropavlovsk-Kamchatsky' },
  { timeZone: 'Etc/GMT-12', description: 'Coordinated Universal Time+12' },
  { timeZone: 'Pacific/Funafuti', description: 'Funafuti' },
  { timeZone: 'Pacific/Kwajalein', description: 'Kwajalein' },
  { timeZone: 'Pacific/Majuro', description: 'Majuro' },
  { timeZone: 'Pacific/Nauru', description: 'Nauru' },
  { timeZone: 'Pacific/Tarawa', description: 'Tarawa' },
  { timeZone: 'Pacific/Wake', description: 'Wake' },
  { timeZone: 'Pacific/Wallis', description: 'Wallis' },
  { timeZone: 'Pacific/Auckland', description: 'Auckland, Wellington' },
  { timeZone: 'Pacific/Fiji', description: 'Fiji' },
  { timeZone: 'Pacific/Chatham', description: 'Chatham Islands' },
  { timeZone: 'Pacific/Enderbury', description: 'Phoenix Islands, Tokelau, Tonga' },
  { timeZone: 'Pacific/Fakaofo', description: 'Fakaofo' },
  { timeZone: 'Pacific/Tongatapu', description: 'Tongatapu' },
  { timeZone: 'Pacific/Apia', description: 'Apia' },
  { timeZone: 'Pacific/Kiritimati', description: 'Line Islands' },
]

export const WHITELISTED_TIME_ZONES = WHITELISTED_TIME_ZONE_INFO.map(({ timeZone }) => timeZone)
export const WHITELISTED_TIME_ZONE_DESCRIPTIONS = WHITELISTED_TIME_ZONE_INFO.map(
  ({ description }) => description,
)

export const WHITELISTED_TIME_ZONE_MAP = new Map(
  WHITELISTED_TIME_ZONE_INFO.map((info) => [info.timeZone, info]),
)

export const WHITELISTED_TIME_ZONE_DESCRIPTION_MAP = new Map(
  WHITELISTED_TIME_ZONE_INFO.map((info) => [info.description, info]),
)

/** A time zone in the IANA time zone database. */
export type IanaTimeZone = Newtype<string, 'IanaTimeZone'>
/** Create a {@link IanaTimeZone}. */
export const IanaTimeZone = newtypeConstructor<IanaTimeZone>()

/**
 * Get the corresponding timezone given its description.
 * @throws {Error} when the description does not correspond to the description for
 * one of the whitelisted timezones.
 */
export function tryGetDescriptionForTimeZone(
  timeZone: string | undefined,
  fallbackTimeZone?: IanaTimeZone,
): string {
  const description = timeZone != null ? WHITELISTED_TIME_ZONE_MAP.get(timeZone)?.description : null
  if (!description) {
    if (fallbackTimeZone) {
      const fallbackDescription = WHITELISTED_TIME_ZONE_MAP.get(fallbackTimeZone)?.description
      if (fallbackDescription) return fallbackDescription
    }
    throw new Error(`Unknown timezone description for IANA identifier '${timeZone}'.`)
  }
  return description
}

/**
 * Get the corresponding timezone given its description.
 * @throws {Error} when the description does not correspond to the description for
 * one of the whitelisted timezones.
 */
export function getDescriptionForTimeZone(timeZone: IanaTimeZone): string {
  return tryGetDescriptionForTimeZone(timeZone)
}

/** Get the corresponding timezone given its description. */
export function tryGetTimeZoneFromDescription(description: string): IanaTimeZone | null {
  const timeZone = WHITELISTED_TIME_ZONE_DESCRIPTION_MAP.get(description)?.timeZone
  return timeZone != null ? IanaTimeZone(timeZone) : null
}

/**
 * Get the corresponding timezone given its description.
 * @throws {Error} when the description does not correspond to the description for
 * one of the whitelisted timezones.
 */
export function getTimeZoneFromDescription(description: string): IanaTimeZone {
  const timeZone = tryGetTimeZoneFromDescription(description)
  if (timeZone == null) {
    throw new Error(`Unknown IANA identifier for timezone '${description}'.`)
  }
  return timeZone
}

/** A string with date and time, following the RFC3339 specification. */
export type Rfc3339DateTime = Newtype<string, 'Rfc3339DateTime'>
/** Create a {@link Rfc3339DateTime}. */
export const Rfc3339DateTime = newtypeConstructor<Rfc3339DateTime>()

/** Format a {@link Date} as a {@link Rfc3339DateTime}. */
export function toRfc3339(date: Date) {
  return Rfc3339DateTime(date.toISOString())
}

/** Format a {@link Date} as a human-readable ISO string (`YYYY-MM-DD HH:mm`). */
export function toReadableIsoString(date: Date, timeZone?: string) {
  const formatter =
    timeZone == null ? ISO_FORMAT : (
      Intl.DateTimeFormat('sv', { dateStyle: 'short', timeStyle: 'short', timeZone })
    )
  return formatter.format(date)
}

/** Format a {@link ZonedDateTime} as a {@link Rfc3339DateTime}. */
export function zonedDateTimeToReadableIsoString(date: ZonedDateTime) {
  const isoString = date.toString()
  const [, dateString, hour, minute] = isoString.match(/(.+)T(\d+):(\d+)/) ?? []
  return `${dateString} ${hour}:${minute}`
}

/** Get a consistent day number for the day of week no matter the locale of the local device. */
export function getDay(date: ZonedDateTime) {
  return getDayOfWeek(date, DAY_OF_WEEK_LOCALE)
}

/**
 * Get the number of times this day of the week has occurred so far this month.
 * Dates 1 through 7 are week 1, 8 through 14 are week 2, etc.
 */
export function getWeekOfMonth(day: number) {
  return Math.floor((day - 1) / DAYS_PER_WEEK) + 1
}

/** Get a string representing the timezone offset of a date. */
export function getTimeZoneOffsetString(date: ZonedDateTime) {
  const offsetMin = date.offset / MINUTE_MS
  const offsetNegative = offsetMin < 0
  const absoluteOffsetMin = Math.abs(offsetMin)
  const offsetHours = Math.floor(absoluteOffsetMin / HOUR_MINUTES)
  const offsetMinutes = absoluteOffsetMin % HOUR_MINUTES
  return `${offsetNegative ? '-' : '+'}${`${offsetHours}`.padStart(2, '0')}:${`${offsetMinutes}`.padStart(2, '0')}`
}

/** Get a string representing the timezone offset of a date, wrapped in `(GMT` and `)`. */
export function getTimeZoneOffsetStringWithGMT(date: ZonedDateTime) {
  return `(GMT${getTimeZoneOffsetString(date)})`
}
