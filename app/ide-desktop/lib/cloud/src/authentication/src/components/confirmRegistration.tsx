import { FC, useEffect } from "react";
import { useLocation, useNavigate } from "react-router-dom";
import { LOGIN_PATH } from "./app";
import withRouter from "../navigation";
import { useAuth } from "../authentication";
import toast from "react-hot-toast";



// ====================================
// === confirmRegistrationContainer ===
// ====================================

const confirmRegistrationContainer: FC = () => {
  const { confirmSignUp } = useAuth();
  const { search } = useLocation();
  const navigate = useNavigate();

  // Parse the verification code & email from the query params.
  // FIXME [NP]: refactor to hook, here, in login, and in reset password
  const query = new URLSearchParams(search);
  const verificationCode = query.get("verification_code");
  const email = query.get("email");

  useEffect(() => {
    if (!email || !verificationCode) {
      navigate(LOGIN_PATH);
      return;
    }

    confirmSignUp(email, verificationCode)
      // FIXME [NP]: encode ONLY email here, not the whole query string
      .then(() => navigate(LOGIN_PATH + search.toString()))
      .catch((error) => {
        // FIXME [NP]: handle this error properly
        console.error(error)
        toast.error("Something went wrong! Please try again or contact the administrators.");
        navigate(LOGIN_PATH);
      })
  }, [])
 
  return (<div></div>);
}

export default withRouter(confirmRegistrationContainer);
