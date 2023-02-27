/** @file Login container responsible for rendering and interactions in sign in flow. */

import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import * as React from 'react'
import { Link, useLocation } from 'react-router-dom';
import { faGoogle, faGithub } from '@fortawesome/free-brands-svg-icons';

import { useAuth } from '../providers/auth';
import { useInput } from '../../hooks';
import withRouter from '../../navigation'
import { handleEvent } from '../../utils';
import { FORGOT_PASSWORD_PATH, REGISTRATION_PATH } from '../../components/app';
import * as Icons from '../../components/svg';



// =================
// === Constants ===
// =================

const EMAIL_QUERY_PARAM = "email";



// ======================
// === loginContainer ===
// ======================

const loginContainer = () => {
    const { search } = useLocation();
    const { signInWithGoogle, signInWithGitHub, signInWithPassword } = useAuth();

    const initialEmail = parseUrlSearchParams(search);

    const { value: email, bind: bindEmail } = useInput(initialEmail ?? "")
    const { value: password, bind: bindPassword } = useInput("")

    return (
      <div className="min-h-screen flex flex-col items-center justify-center bg-gray-300">
        <div className="flex flex-col bg-white shadow-md px-4 sm:px-6 md:px-8 lg:px-10 py-8 rounded-md w-full max-w-md">
          <div className="font-medium self-center text-xl sm:text-2xl uppercase text-gray-800">
            Login To Your Account
          </div>
          <button
            onClick={handleEvent(signInWithGoogle)}
            className="relative mt-6 border rounded-md py-2 text-sm text-gray-800 bg-gray-100 hover:bg-gray-200"
          >
            <span className="absolute left-0 top-0 flex items-center justify-center h-full w-10 text-blue-500">
              <FontAwesomeIcon icon={faGoogle} />
            </span>
            <span>Login with Google</span>
          </button>
          <button
            onClick={handleEvent(signInWithGitHub)}
            className="relative mt-6 border rounded-md py-2 text-sm text-gray-800 bg-gray-100 hover:bg-gray-200"
          >
            <span className="absolute left-0 top-0 flex items-center justify-center h-full w-10 text-blue-500">
              <FontAwesomeIcon icon={faGithub} />
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
            <form onSubmit={handleEvent(async () => signInWithPassword(email, password))}>
              <div className="flex flex-col mb-6">
                <label
                  htmlFor="email"
                  className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
                >
                  E-Mail Address:
                </label>
                <div className="relative">
                  <div className="inline-flex items-center justify-center absolute left-0 top-0 h-full w-10 text-gray-400">
                    <span><Icons.Svg data={Icons.PATHS.at} /></span>
                  </div>

                  <input
                    {...bindEmail}
                    required={true}
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
                    <span><Icons.Svg data={Icons.PATHS.lock} /></span>
                  </div>

                  <input
                    {...bindPassword}
                    required={true}
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
                    to={FORGOT_PASSWORD_PATH}
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
                  <span><Icons.Svg data={Icons.PATHS.rightArrow} /></span>
                </button>
              </div>
            </form>
          </div>
          <div className="flex justify-center items-center mt-6">
            <Link
              to={REGISTRATION_PATH}
              className="inline-flex items-center font-bold text-blue-500 hover:text-blue-700 text-xs text-center"
            >
              <span><Icons.Svg data={Icons.PATHS.createAccount} /></span>
              <span className="ml-2">You don&apos;t have an account?</span>
            </Link>
          </div>
        </div>
      </div>
    );
}

const parseUrlSearchParams = (search: string) => {
  const query = new URLSearchParams(search);
  const email = query.get(EMAIL_QUERY_PARAM);
  return email
}



export default withRouter(loginContainer)
