const errorMessage = "is not set or empty.";

if (!process.env.NEXT_PUBLIC_AUTH_REGION) {
  throw new Error(`NEXT_PUBLIC_AUTH_REGION ${errorMessage}`);
}

if (!process.env.NEXT_PUBLIC_AUTH_USER_POOL_ID) {
  throw new Error(`NEXT_PUBLIC_AUTH_USER_POOL_ID ${errorMessage}`);
}

if (!process.env.NEXT_PUBLIC_AUTH_USER_POOL_WEB_CLIENT_ID) {
  throw new Error(`NEXT_PUBLIC_AUTH_USER_POOL_WEB_CLIENT_ID ${errorMessage}`);
}

if (!process.env.NEXT_PUBLIC_AUTH_DOMAIN) {
  throw new Error(`NEXT_PUBLIC_AUTH_DOMAIN ${errorMessage}`);
}

if (!process.env.NEXT_PUBLIC_AUTH_REDIRECT_SIGN_IN) {
  throw new Error(`NEXT_PUBLIC_AUTH_REDIRECT_SIGN_IN ${errorMessage}`);
}

if (!process.env.NEXT_PUBLIC_AUTH_REDIRECT_SIGN_OUT) {
  throw new Error(`NEXT_PUBLIC_AUTH_REDIRECT_SIGN_OUT ${errorMessage}`);
}

// =====================
// === amplifyConfig ===
// =====================

const REGION = process.env.NEXT_PUBLIC_AUTH_REGION;
const USER_POOL_ID = process.env.NEXT_PUBLIC_AUTH_USER_POOL_ID;
const USER_POOL_WEB_CLIENT_ID =
  process.env.NEXT_PUBLIC_AUTH_USER_POOL_WEB_CLIENT_ID;
const DOMAIN = process.env.NEXT_PUBLIC_AUTH_DOMAIN;
const REDIRECT_SIGN_IN = process.env.NEXT_PUBLIC_AUTH_REDIRECT_SIGN_IN;
const REDIRECT_SIGN_OUT = process.env.NEXT_PUBLIC_AUTH_REDIRECT_SIGN_OUT;

export const amplifyConfig = {
  region: REGION,
  userPoolId: USER_POOL_ID,
  userPoolWebClientId: USER_POOL_WEB_CLIENT_ID,
  oauth: {
    domain: DOMAIN,
    redirectSignIn: REDIRECT_SIGN_IN,
    redirectSignOut: REDIRECT_SIGN_OUT,
    responseType: "code",
  },
};
