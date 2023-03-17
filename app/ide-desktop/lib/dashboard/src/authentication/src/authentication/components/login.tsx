/** @file Login component responsible for rendering and interactions in sign in flow. */
import * as router from "react-router-dom";

import * as app from "../../components/app";
import * as icons from "../../components/svg";

// =============
// === Login ===
// =============

function Login() {
  return (
    <div className="min-h-screen flex flex-col items-center justify-center bg-gray-300">
      <div
        className={
          "flex flex-col bg-white shadow-md px-4 sm:px-6 md:px-8 lg:px-10 py-8 rounded-md w-full " +
          "max-w-md"
        }
      >
        <div className="flex justify-center items-center mt-6">
          <router.Link
            to={app.REGISTRATION_PATH}
            className={
              "inline-flex items-center font-bold text-blue-500 hover:text-blue-700 text-xs " +
              "text-center"
            }
          >
            <span>
              <icons.Svg data={icons.PATHS.createAccount} />
            </span>
            <span className="ml-2">You don&apos;t have an account?</span>
          </router.Link>
        </div>
      </div>
    </div>
  );
}

export default Login;
