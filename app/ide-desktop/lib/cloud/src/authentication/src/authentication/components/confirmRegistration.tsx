import { useEffect } from "react";
import { useLocation, useNavigate } from "react-router-dom";
import { LOGIN_PATH } from "../../components/app";
import withRouter from "../../navigation";
import { useAuth } from "../providers/auth";
import toast from "react-hot-toast";
import { useLogger } from "../../providers/logger";



// =================
// === Constants ===
// =================

export const VERIFICATION_CODE_QUERY_PARAM = "verification_code";
export const EMAIL_QUERY_PARAM = "email";



// ====================================
// === confirmRegistrationContainer ===
// ====================================

const confirmRegistrationContainer = () => {
  const logger = useLogger();
  const { confirmSignUp } = useAuth();
  const { search } = useLocation();
  const navigate = useNavigate();

  const { verificationCode, email } = parseUrlSearchParams(search);

  useEffect(() => {
    if (!email || !verificationCode) {
      navigate(LOGIN_PATH);
      return;
    }

    confirmSignUp(email, verificationCode)
      .then(() => navigate(LOGIN_PATH + search.toString()))
      .catch((error) => {
        logger.error("Error while confirming sign-up", error)
        toast.error("Something went wrong! Please try again or contact the administrators.");
        navigate(LOGIN_PATH);
      })
  }, [])
 
  return (<div></div>);
}

const parseUrlSearchParams = (search: string) => {
  const query = new URLSearchParams(search);
  const verificationCode = query.get(VERIFICATION_CODE_QUERY_PARAM);
  const email = query.get(EMAIL_QUERY_PARAM);
  return { verificationCode, email }
}


export default withRouter(confirmRegistrationContainer);
