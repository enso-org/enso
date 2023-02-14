import { Auth } from "aws-amplify";
import { toast } from "react-hot-toast";
import { FC, FormEvent } from "react";
import { useLocation, useNavigate } from "react-router-dom";
import { LOGIN_PATH } from "./app";
import withRouter from "../navigation";



// ====================================
// === confirmRegistrationContainer ===
// ====================================

const confirmRegistrationContainer: FC<any> = () => {
  const { search }= useLocation();
  const navigate = useNavigate();

  // Parse the verification code & email from the query params.
  const query = new URLSearchParams(search);
  const verificationCode = query.get("verification_code");
  const email = query.get("email");

  if (!email || !verificationCode) {
    // FIXME [NP]: we used to pass ?confirmed=false, make sure this still works.
    navigate(LOGIN_PATH, { state: { confirmed: false } });
  } else {
    // FIXME [NP]: hide the library details
    console.log("Confirming registration for user: ", email, verificationCode)
    Auth
      .confirmSignUp(email, verificationCode)
      // FIXME [NP]: we used to pass ?confirmed=true, make sure this still works.
      .then(() => navigate(LOGIN_PATH, { state: { confirmed: true } }))
      .then(() => toast.success("Your account has been confirmed! Please log in."))
      // FIXME [NP]: ensure we handle the error appropriately.
      .catch((error) => {
        console.error(error);
        toast.error("Something went wrong! Please try again or contact the administrators.");
      })
  }

 
  // FIXME [NP]: maybe take the user to the login page straight away?
  return (<div></div>);
}

export default withRouter(confirmRegistrationContainer)
