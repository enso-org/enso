import { Auth } from "aws-amplify";
import Link from "next/link";
import { CognitoHostedUIIdentityProvider } from "@aws-amplify/auth";
import { useInput } from "../service";
import { useRouter } from "next/router";
import toast from "react-hot-toast";
import { useEffect } from "react";
// FIXME [NP]: remove this
import * as authentication from "enso-studio-authentication";

authentication.run(console);

function Login() {
  let router = useRouter();

  useEffect(() => {
    if (router.query.confirmed === "true") {
      toast.success("Confirmation success! You may now login.");
    } else if (router.query.confirmed === "false") {
      toast.error(
        "Something went wrong! Please try again or contact the administrators: cloud@enso.org."
      );
    }
  }, [router]);

  const signInWithGoogle = () =>
    Auth.federatedSignIn({ provider: CognitoHostedUIIdentityProvider.Google });

  const signInWithGithub = () =>
    Auth.federatedSignIn({ customProvider: "Github" });

  const signInWithPassword = async (
    e: React.SyntheticEvent<Element, Event>
  ) => {
    e.preventDefault();

    try {
      await Auth.signIn(email, password);
      await router.push("/");
    } catch (error) {
      toast.error(error.message);
    }
  };

  const { value: email, bind: bindEmail } = useInput("");
  const { value: password, bind: bindPassword } = useInput("");

  return (
    <div className="min-h-screen flex flex-col items-center justify-center bg-gray-300">
      <div className="flex flex-col bg-white shadow-md px-4 sm:px-6 md:px-8 lg:px-10 py-8 rounded-md w-full max-w-md">
        <div className="font-medium self-center text-xl sm:text-2xl uppercase text-gray-800">
          Login To Your Account
        </div>
        <button
          onClick={signInWithGoogle}
          className="relative mt-6 border rounded-md py-2 text-sm text-gray-800 bg-gray-100 hover:bg-gray-200"
        >
          <span className="absolute left-0 top-0 flex items-center justify-center h-full w-10 text-blue-500">
            {/*<FontAwesomeIcon icon={['fab', 'google']} />*/}
          </span>
          <span>Login with Google</span>
        </button>
        <button
          onClick={signInWithGithub}
          className="relative mt-6 border rounded-md py-2 text-sm text-gray-800 bg-gray-100 hover:bg-gray-200"
        >
          <span className="absolute left-0 top-0 flex items-center justify-center h-full w-10 text-blue-500">
            {/*<FontAwesomeIcon icon={["fab", "github"]} />*/}
          </span>
          <span>Login with Github</span>
        </button>
        <div className="relative mt-10 h-px bg-gray-300">
          <div className="absolute left-0 top-0 flex justify-center w-full -mt-2">
            <span className="bg-white px-4 text-xs text-gray-500 uppercase">
              Or Login With Email
            </span>
          </div>
        </div>
        <div className="mt-10">
          <form onSubmit={signInWithPassword}>
            <div className="flex flex-col mb-6">
              <label
                htmlFor="email"
                className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
              >
                E-Mail Address:
              </label>
              <div className="relative">
                <div className="inline-flex items-center justify-center absolute left-0 top-0 h-full w-10 text-gray-400">
                  <svg
                    className="h-6 w-6"
                    fill="none"
                    strokeLinecap="round"
                    strokeLinejoin="round"
                    strokeWidth="2"
                    viewBox="0 0 24 24"
                    stroke="currentColor"
                  >
                    <path d="M16 12a4 4 0 10-8 0 4 4 0 008 0zm0 0v1.5a2.5 2.5 0 005 0V12a9 9 0 10-9 9m4.5-1.206a8.959 8.959 0 01-4.5 1.207" />
                  </svg>
                </div>

                <input
                  {...bindEmail}
                  id="email"
                  type="email"
                  name="email"
                  className="text-sm sm:text-base placeholder-gray-500 pl-10 pr-4 rounded-lg border border-gray-400 w-full py-2 focus:outline-none focus:border-blue-400"
                  placeholder="E-Mail Address"
                />
              </div>
            </div>
            <div className="flex flex-col mb-6">
              <label
                htmlFor="password"
                className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
              >
                Password:
              </label>
              <div className="relative">
                <div className="inline-flex items-center justify-center absolute left-0 top-0 h-full w-10 text-gray-400">
                  <span>
                    <svg
                      className="h-6 w-6"
                      fill="none"
                      strokeLinecap="round"
                      strokeLinejoin="round"
                      strokeWidth="2"
                      viewBox="0 0 24 24"
                      stroke="currentColor"
                    >
                      <path d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z" />
                    </svg>
                  </span>
                </div>

                <input
                  {...bindPassword}
                  id="password"
                  type="password"
                  name="password"
                  className="text-sm sm:text-base placeholder-gray-500 pl-10 pr-4 rounded-lg border border-gray-400 w-full py-2 focus:outline-none focus:border-blue-400"
                  placeholder="Password"
                />
              </div>
            </div>

            <div className="flex items-center mb-6 -mt-4">
              <div className="flex ml-auto">
                <Link
                  href="/forgot-password"
                  className="inline-flex text-xs sm:text-sm text-blue-500 hover:text-blue-700"
                >
                  Forgot Your Password?
                </Link>
              </div>
            </div>

            <div className="flex w-full">
              <button
                type="submit"
                className="flex items-center justify-center focus:outline-none text-white text-sm sm:text-base bg-blue-600 hover:bg-blue-700 rounded py-2 w-full transition duration-150 ease-in"
              >
                <span className="mr-2 uppercase">Login</span>
                <span>
                  <svg
                    className="h-6 w-6"
                    fill="none"
                    strokeLinecap="round"
                    strokeLinejoin="round"
                    strokeWidth="2"
                    viewBox="0 0 24 24"
                    stroke="currentColor"
                  >
                    <path d="M13 9l3 3m0 0l-3 3m3-3H8m13 0a9 9 0 11-18 0 9 9 0 0118 0z" />
                  </svg>
                </span>
              </button>
            </div>
          </form>
        </div>
        <div className="flex justify-center items-center mt-6">
          <Link
            href="/registration"
            className="inline-flex items-center font-bold text-blue-500 hover:text-blue-700 text-xs text-center"
          >
            <span>
              <svg
                className="h-6 w-6"
                fill="none"
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth="2"
                viewBox="0 0 24 24"
                stroke="currentColor"
              >
                <path d="M18 9v3m0 0v3m0-3h3m-3 0h-3m-2-5a4 4 0 11-8 0 4 4 0 018 0zM3 20a6 6 0 0112 0v1H3v-1z" />
              </svg>
            </span>
            <span className="ml-2">You don&apos;t have an account?</span>
          </Link>
        </div>
      </div>
    </div>
  );
}

Login.isAuthComponent = true;
export default Login;
