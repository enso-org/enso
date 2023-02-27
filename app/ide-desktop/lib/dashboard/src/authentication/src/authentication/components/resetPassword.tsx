/**
 * @file Container responsible for rendering and interactions in second half of forgot password
 * flow.
 */
import * as React from 'react'
import { Link, useLocation } from 'react-router-dom';
import toast from "react-hot-toast";

import { useAuth } from '../providers/auth';
import withRouter from '../../navigation'
import { useInput } from '../../hooks'
import { handleEvent } from '../../utils';
import { LOGIN_PATH } from '../../components/app';
import * as Icons from '../../components/svg';
import { EMAIL_QUERY_PARAM, VERIFICATION_CODE_QUERY_PARAM } from './confirmRegistration';



// ==============================
// === resetPasswordContainer ===
// ==============================

const resetPasswordContainer = () => {
    const { resetPassword } = useAuth();
    const { search } = useLocation();

    const { verificationCode: initialCode, email: initialEmail } = parseUrlSearchParams(search);

    const { value: email, bind: bindEmail } = useInput(initialEmail ?? "")
    const { value: code, bind: bindCode } = useInput(initialCode ?? "");
    const { value: newPassword, bind: bindNewPassword } = useInput("");
    const { value: newPasswordConfirm, bind: bindNewPasswordConfirm } = useInput("");

    const handleSubmit = () => {
        if (newPassword !== newPasswordConfirm) {
            toast.error("Passwords do not match");
            return Promise.resolve();
        }

        return resetPassword(email, code, newPassword)
    };

    return (
      <div className="min-h-screen flex flex-col items-center justify-center bg-gray-300">
        <div className="flex flex-col bg-white shadow-md px-4 sm:px-6 md:px-8 lg:px-10 py-8 rounded-md w-full max-w-md">
          <div className="font-medium self-center text-xl sm:text-2xl uppercase text-gray-800">
            Reset Your Password
          </div>
          <div className="mt-10">
            <form onSubmit={handleEvent(handleSubmit)}>
              <div className="flex flex-col mb-6">
                <label
                  htmlFor="email"
                  className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
                >
                  E-Mail Address:
                </label>
                <div className="relative">
                  <div className="inline-flex items-center justify-center absolute left-0 top-0 h-full w-10 text-gray-400">
                    <Icons.Svg data={Icons.PATHS.at} />
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
                  htmlFor="code"
                  className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
                >
                  Confirmation Code:
                </label>
                <div className="relative">
                  <div className="inline-flex items-center justify-center absolute left-0 top-0 h-full w-10 text-gray-400">
                    <Icons.Svg data={Icons.PATHS.lock} />
                  </div>
  
                  <input
                    {...bindCode}
                    id="code"
                    type="text"
                    name="code"
                    className="text-sm sm:text-base placeholder-gray-500 pl-10 pr-4 rounded-lg border border-gray-400 w-full py-2 focus:outline-none focus:border-blue-400"
                    placeholder="Confirmation Code"
                  />
                </div>
              </div>
              <div className="flex flex-col mb-6">
                <label
                  htmlFor="new_password"
                  className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
                >
                  New Password:
                </label>
                <div className="relative">
                  <div className="inline-flex items-center justify-center absolute left-0 top-0 h-full w-10 text-gray-400">
                    <Icons.Svg data={Icons.PATHS.lock} />
                  </div>
  
                  <input
                    {...bindNewPassword}
                    id="new_password"
                    type="password"
                    name="new_password"
                    className="text-sm sm:text-base placeholder-gray-500 pl-10 pr-4 rounded-lg border border-gray-400 w-full py-2 focus:outline-none focus:border-blue-400"
                    placeholder="New Password"
                  />
                </div>
              </div>
              <div className="flex flex-col mb-6">
                <label
                  htmlFor="new_password_confirm"
                  className="mb-1 text-xs sm:text-sm tracking-wide text-gray-600"
                >
                  Confirm New Password:
                </label>
                <div className="relative">
                  <div className="inline-flex items-center justify-center absolute left-0 top-0 h-full w-10 text-gray-400">
                    <Icons.Svg data={Icons.PATHS.lock} />
                  </div>
  
                  <input
                    {...bindNewPasswordConfirm}
                    id="new_password_confirm"
                    type="password"
                    name="new_password_confirm"
                    className="text-sm sm:text-base placeholder-gray-500 pl-10 pr-4 rounded-lg border border-gray-400 w-full py-2 focus:outline-none focus:border-blue-400"
                    placeholder="Confirm New Password"
                  />
                </div>
              </div>
              <div className="flex w-full">
                <button
                  type="submit"
                  className="flex items-center justify-center focus:outline-none text-white text-sm sm:text-base bg-blue-600 hover:bg-blue-700 rounded py-2 w-full transition duration-150 ease-in"
                >
                  <span className="mr-2 uppercase">Reset</span>
                  <span><Icons.Svg data={Icons.PATHS.rightArrow} /></span>
                </button>
              </div>
            </form>
          </div>
          <div className="flex justify-center items-center mt-6">
            <Link
              to={LOGIN_PATH}
              className="inline-flex items-center font-bold text-blue-500 hover:text-blue-700 text-xs text-center"
            >
              <span><Icons.Svg data={Icons.PATHS.goBack} /></span>
              <span className="ml-2">Go back to login</span>
            </Link>
          </div>
        </div>
      </div>
    );
}

const parseUrlSearchParams = (search: string) => {
    const query = new URLSearchParams(search);
    const verificationCode = query.get(VERIFICATION_CODE_QUERY_PARAM);
    const email = query.get(EMAIL_QUERY_PARAM);
    return { verificationCode, email }
}

export default withRouter(resetPasswordContainer)


