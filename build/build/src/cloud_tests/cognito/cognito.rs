//! Adapted from https://github.com/enso-org/cloud-v2/blob/2e956fe89636b62c1c4683293c0e1572b4b088c2/src/lambda_integration_tests/src/cognito.rs
//! 
//! Module containing a minimal implementation of an AWS Cognito flow to authenticate a user and
//! acquire a JWT token that can be used to authenticate further requests by that user.
//!
//! The API is intended to be side-effect free for ease of testing. The entrypoint is
//! [`InitiateAuthStage::new`], which takes the parameters required to begin the flow. The flow is
//! then completed by calling [`InitiateAuthStage::run_to_end`], which executes the HTTP requests of
//! the flow in sequence and returns a [`Finalized`] struct containing an `access_token`.
//!
//! Most of this file is a reimplementation of Amazon's [`AuthenticationHelper.js`] (and the
//! equivalent Java code). For details on the specifics of the cryptography involved, see that file.
//!
//! [`InitiateAuthStage::new`]: crate::cognito::InitiateAuthStage::new
//! [`InitiateAuthStage::run_to_end`]: crate::cognito::InitiateAuthStage::run_to_end
//! [`Finalized`]: crate::cognito::Finalized
//! [`AuthenticationHelper.js`]: https://github.com/amazon-archives/amazon-cognito-identity-js/blob/master/src/AuthenticationHelper.js

// Copyright 2016 Amazon.com,
// Inc. or its affiliates. All Rights Reserved.
//
// Licensed under the Amazon Software License (the "License").
// You may not use this file except in compliance with the
// License. A copy of the License is located at
//
//     http://aws.amazon.com/asl/
//
// or in the "license" file accompanying this file. This file is
// distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, express or implied. See the License
// for the specific language governing permissions and
// limitations under the License.

use crate::prelude::*;

use core::future;
use num::bigint;
use ring::digest;
use ring::hmac;



// =================
// === Constants ===
// =================

/// Radix used to parse a hex string into an integer.
const HEX_RADIX: u32 = 16;
/// Number of random bytes required to initialize the `a` parameter of the SRP flow.
pub(crate) const SMALL_A_LEN: usize = 128;
/// Length of the hashed and signed key, in bytes.
const HKDF_LEN: usize = 16;

fn hex_to_big(hex_str: &str) -> num::BigInt {
    num::BigInt::parse_bytes(hex_str.as_bytes(), HEX_RADIX).expect("Error parsing hex string.")
}

fn calculate_random_small_a(rand_bytes: &[u8; SMALL_A_LEN], big_n: &num::BigInt) -> num::BigInt {
    let rand_big = num::BigInt::from_bytes_be(bigint::Sign::Plus, rand_bytes);
    rand_big % big_n
}

fn calculate_a(val_g: &num::BigInt, small_a: &num::BigInt, big_n: &num::BigInt) -> num::BigInt {
    let big_a = val_g.modpow(small_a, big_n);
    if &big_a % big_n == num::BigInt::from(0u32) {
        panic!("Safety check for A failed.");
    }
    big_a
}

fn big_to_hex(big: &num::BigInt) -> String {
    big.to_str_radix(HEX_RADIX)
}

fn pad_hex_str(input: &str) -> String {
    if input.len() % 2 == 1 {
        format!("0{input}")
    } else if "89ABCDEFabcdef"
        .find(input.chars().next().expect("Zero length string can not be padded."))
        .is_some()
    {
        format!("00{input}")
    } else {
        input.to_string()
    }
}

fn pad_hex_big(input: &num::BigInt) -> String {
    let str = big_to_hex(input);
    pad_hex_str(&str)
}

fn hash_sha256(buf: &[u8]) -> String {
    let sha = digest::digest(&digest::SHA256, buf);
    format!("{:064}", hex::encode(sha.as_ref()))
}

fn hex_hash(hex_str: &str) -> String {
    let bytes = hex::decode(hex_str).expect("Invalid hex string provided for decoding.");
    hash_sha256(&bytes)
}

fn calculate_u(srp_a: &num::BigInt, srp_b: &num::BigInt) -> num::BigInt {
    let u_hex_hash = hex_hash(&format!("{}{}", pad_hex_big(srp_a), pad_hex_big(srp_b)));
    hex_to_big(&u_hex_hash)
}

fn compute_hkdf(ikm: &[u8], salt: &[u8]) -> [u8; HKDF_LEN] {
    /// Constant info bits value, taken directly from the AWS Cognito implementation. Should not be
    /// changed, otherwise the HKDF will not be compatible with AWS Cognito.
    const INFO_BITS: &str = "Caldera Derived Key\x01";
    let key = hmac::Key::new(hmac::HMAC_SHA256, salt);
    let tag = hmac::sign(&key, ikm);
    let final_key = hmac::Key::new(hmac::HMAC_SHA256, tag.as_ref());
    let final_tag = hmac::sign(&final_key, INFO_BITS.as_bytes());
    *arrayref::array_ref!(final_tag.as_ref(), 0, HKDF_LEN)
}



// ===============
// === KeyData ===
// ===============

#[derive(Clone, Debug)]
struct KeyData {
    val_g:   num::BigInt,
    big_n:   num::BigInt,
    val_k:   num::BigInt,
    small_a: num::BigInt,
    big_a:   num::BigInt,
}


// === Internal `impl` ===

impl KeyData {
    fn new(rand_bytes: &[u8; SMALL_A_LEN]) -> Self {
        /// The `N` cryptographic constant value used in the AWS Cognito SRP flow.
        const N_HEX: &str = "FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC\
        74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B5766\
        25E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC200\
        7CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966\
        D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F\
        4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AAAC42DAD33170D04507A33A8552\
        1ABDF1CBA64ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7ABF5AE8CDB0933D71E8C94E04A25619D\
        CEE3D2261AD2EE6BF12FFA06D98A0864D87602733EC86A64521F2B18177B200CBBE117577A615D6C770988C0BAD\
        946E208E24FA074E5AB3143DB5BFCE0FD108E4B82D120A93AD2CAFFFFFFFFFFFFFFFF";
        /// The `G` cryptographic constant value used in the AWS Cognito SRP flow.
        const G_HEX: &str = "2";
        /// The `K` cryptographic constant value used in the AWS Cognito SRP flow.
        const K_HEX: &str = concatcp!("00", N_HEX, "0", G_HEX);

        let val_g = hex_to_big(G_HEX);
        let big_n = hex_to_big(N_HEX);
        let val_k = hex_to_big(&hex_hash(K_HEX));

        let small_a = calculate_random_small_a(rand_bytes, &big_n);
        let big_a = calculate_a(&val_g, &small_a, &big_n);

        Self { val_g, big_n, val_k, small_a, big_a }
    }

    fn get_password_authentication_key(
        &self,
        pool_id: &str,
        username: &str,
        password: &str,
        srp_b: &num::BigInt,
        salt: &num::BigInt,
    ) -> [u8; HKDF_LEN] {
        let u = calculate_u(&self.big_a, srp_b);
        if u == num::BigInt::from(0u32) {
            panic!("U can not be zero.");
        }

        let user_pass = format!("{pool_id}{username}:{password}");
        let user_pass_digest = hash_sha256(user_pass.as_bytes());

        let x = hex_to_big(&pad_hex_str(&hex_hash(&format!(
            "{}{user_pass_digest}",
            pad_hex_big(salt),
        ))));
        let g_mod_pow_xn = self.val_g.modpow(&x, &self.big_n);

        let int_value2 = srp_b - &self.val_k * &g_mod_pow_xn;

        let s =
            int_value2.modpow(&hex_to_big(&pad_hex_big(&(&self.small_a + &u * &x))), &self.big_n);

        compute_hkdf(
            &hex::decode(pad_hex_big(&s)).expect("Error decoding `s` value."),
            &hex::decode(pad_hex_big(&u)).expect("Error decoding `u` value."),
        )
    }
}

/// Creates an HTTP request to initialize the Cognito auth flow.
fn initiate_auth_request(
    api_url: &str,
    username: &str,
    client_id: &str,
    srp_a: &str,
) -> http::Request<Vec<u8>> {
    http::Request::builder()
        .method(http::Method::POST)
        .uri(api_url)
        .header("Content-Type", "application/x-amz-json-1.1")
        .header("X-Amz-Target", "AWSCognitoIdentityProviderService.InitiateAuth")
        .body(
            format!(
                "{{\
          \"AuthFlow\":\"USER_SRP_AUTH\",\
          \"ClientId\":\"{client_id}\",\
          \"AuthParameters\":{{\
            \"USERNAME\":\"{username}\",\
            \"SRP_A\":\"{srp_a}\"\
          }},\
          \"ClientMetadata\":{{}}\
        }}"
            )
            .into_bytes(),
        )
        .unwrap()
}



// =================
// === Challenge ===
// =================

#[derive(Clone, Debug, serde::Deserialize)]
struct Challenge {
    #[serde(rename = "ChallengeName")]
    name:   String,
    #[serde(rename = "ChallengeParameters")]
    params: Parameters,
}

#[derive(Clone, Debug, serde::Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
struct Parameters {
    salt:            String,
    secret_block:    String,
    srp_b:           String,
    username:        String,
    user_id_for_srp: String,
}


// === Internal `impl` ===

impl Challenge {
    fn from_response(response: http::Response<Vec<u8>>) -> Self {
        let (parts, body) = response.into_parts();
        assert_eq!(parts.status, http::StatusCode::OK);
        let deserializer = &mut serde_json::Deserializer::from_slice(&body);
        serde_path_to_error::deserialize(deserializer).unwrap()
    }
}

/// Creates an HTTP request to AWS Cognito to respond to an authentication challenge.
#[allow(clippy::too_many_arguments)]
fn respond_to_auth_challenge_request(
    secret_block: &str,
    client_id: &str,
    username: &str,
    user_id_for_srp: &str,
    hkdf: [u8; HKDF_LEN],
    pool_id: &str,
    timestamp: chrono::DateTime<chrono::Utc>,
    api_url: &str,
) -> http::Request<Vec<u8>> {
    let secret_block_bytes = base64::decode(secret_block).expect("Error decoding secret block.");
    let timestamp = timestamp.format("%a %b %-d %H:%M:%S %Z %Y").to_string();
    let msg =
        [pool_id.as_bytes(), user_id_for_srp.as_bytes(), &secret_block_bytes, timestamp.as_bytes()]
            .concat();
    let key = hmac::Key::new(hmac::HMAC_SHA256, &hkdf);
    let tag = hmac::sign(&key, &msg);
    let signature = base64::encode(tag);
    let body = format!(
        "{{\
    \"ChallengeName\":\"PASSWORD_VERIFIER\",\
    \"ClientId\":\"{client_id}\",\
    \"ChallengeResponses\":{{\
        \"USERNAME\":\"{username}\",
        \"PASSWORD_CLAIM_SECRET_BLOCK\":\"{secret_block}\",\
        \"TIMESTAMP\":\"{timestamp}\",\
        \"PASSWORD_CLAIM_SIGNATURE\":\"{signature}\"\
    }},\
    \"ClientMetadata\":{{}}\
    }}",
    );

    http::Request::builder()
        .method(http::Method::POST)
        .uri(api_url)
        .header("Content-Type", "application/x-amz-json-1.1")
        .header("X-Amz-Target", "AWSCognitoIdentityProviderService.RespondToAuthChallenge")
        .body(body.into_bytes())
        .unwrap()
}



// ======================
// === Authentication ===
// ======================

#[derive(Clone, Debug, serde::Deserialize)]
#[serde(rename_all = "PascalCase")]
struct Authentication {
    authentication_result: AuthenticationResult,
}

#[derive(Clone, Debug, serde::Deserialize)]
#[serde(rename_all = "PascalCase")]
struct AuthenticationResult {
    access_token:  String,
    expires_in:    i64,
    _id_token:      String,
    refresh_token: String,
    _token_type:    String,
}


// === Internal `impl` ===

impl Authentication {
    fn from_response(response: http::Response<Vec<u8>>) -> Self {
        let (parts, body) = response.into_parts();
        assert_eq!(
            parts.status,
            http::StatusCode::OK,
            "Invalid status: {:#?}",
            String::from_utf8(body).unwrap()
        );
        let deserializer = &mut serde_json::Deserializer::from_slice(&body);
        serde_path_to_error::deserialize(deserializer).unwrap()
    }
}



// =========================
// === InitiateAuthStage ===
// =========================

/// Contains initial data for an AWS Cognito authentication flow.
#[derive(Clone, Debug)]
pub(crate) struct InitiateAuthStage {
    key_data:  KeyData,
    api_url:   String,
    client_id: String,
    username:  String,
    password:  String,
    pool_id:   String,
}


// === Main `impl` ===

impl InitiateAuthStage {
    /// Initializes a new AWS Cognito authentication flow, which can be completed using
    /// [`InitiateAuthStage::run_to_end`] to generate a JWT token from the given credentials.
    ///
    /// [`InitiateAuthStage::run_to_end`]: crate::cognito::InitiateAuthStage::run_to_end
    pub(crate) fn new(
        rand_bytes: &[u8; SMALL_A_LEN],
        api_url: String,
        client_id: String,
        username: String,
        password: String,
        cognito_user_pool_id: &str,
    ) -> Self {
        let key_data = KeyData::new(rand_bytes);
        let (_, pool_id) = cognito_user_pool_id.split_once('_').unwrap();

        Self { key_data, api_url, client_id, username, password, pool_id: pool_id.to_string() }
    }

    /// Runs this authentication flow to completion using the provided `req_fn` to execute HTTP
    /// requests, and returns the final state of the authentication flow.
    pub(crate) async fn run_to_end<F, E>(
        self,
        req_fn: impl Fn(http::Request<Vec<u8>>) -> F,
        timestamp: chrono::DateTime<chrono::Utc>,
    ) -> Result<Finalized, E>
    where
        F: future::Future<Output = Result<http::Response<Vec<u8>>, E>>,
    {
        let res = (req_fn)(self.generate_initiate_auth_request()).await?;
        let challenge = self.validate_challenge(res);
        let res = (req_fn)(challenge.generate_respond_to_auth_challenge_request(timestamp)).await?;
        Ok(challenge.validate_authentication(res))
    }
}


// === Internal `impl` ===

impl InitiateAuthStage {
    fn generate_initiate_auth_request(&self) -> http::Request<Vec<u8>> {
        let srp_a = big_to_hex(&self.key_data.big_a);
        initiate_auth_request(&self.api_url, &self.username, &self.client_id, &srp_a)
    }

    fn validate_challenge(self, input: http::Response<Vec<u8>>) -> ChallengeStage {
        let challenge = Challenge::from_response(input);
        assert_eq!(challenge.name, "PASSWORD_VERIFIER");
        ChallengeStage { auth_data: self, challenge }
    }
}



// ======================
// === ChallengeStage ===
// ======================

#[derive(Clone, Debug)]
struct ChallengeStage {
    auth_data: InitiateAuthStage,
    challenge: Challenge,
}


// === Internal `impl` ===

impl ChallengeStage {
    fn generate_respond_to_auth_challenge_request(
        &self,
        timestamp: chrono::DateTime<chrono::Utc>,
    ) -> http::Request<Vec<u8>> {
        let hkdf = self.auth_data.key_data.get_password_authentication_key(
            &self.auth_data.pool_id,
            &self.challenge.params.user_id_for_srp,
            &self.auth_data.password,
            &hex_to_big(&self.challenge.params.srp_b),
            &hex_to_big(&self.challenge.params.salt),
        );
        respond_to_auth_challenge_request(
            &self.challenge.params.secret_block,
            &self.auth_data.client_id,
            &self.challenge.params.username,
            &self.challenge.params.user_id_for_srp,
            hkdf,
            &self.auth_data.pool_id,
            timestamp,
            &self.auth_data.api_url,
        )
    }

    fn validate_authentication(self, input: http::Response<Vec<u8>>) -> Finalized {
        let authentication = Authentication::from_response(input);
        Finalized {
            access_token:  authentication.authentication_result.access_token,
            expires_in:    authentication.authentication_result.expires_in,
            refresh_token: authentication.authentication_result.refresh_token,
        }
    }
}



// =================
// === Finalized ===
// =================

/// Contains the results of a successful AWS Cognito authentication flow.
#[derive(Clone, Debug)]
pub(crate) struct Finalized {
    /// JWT access token from AWS Cognito, used to authenticate a user.
    pub(crate) access_token: String,
    /// Expiration time of the access token, in seconds.
    pub(crate) expires_in: i64,
    /// Refresh token from AWS Cognito, used to refresh the access token.
    pub(crate) refresh_token: String,
}
