import mixpanel from "mixpanel-browser";

const token = process.env.NEXT_PUBLIC_MIXPANEL_TOKEN;

class MixpanelClient {
  private is_prod: boolean;
  public client: any;
  public events: any = {
    PROJECT_CREATED: "Project created.",
    PROJECT_REMOVED: "Project removed.",
    PROJECT_OPEN_SUCCESS: "Project opened.",
    PROJECT_OPEN_FAIL: "Project open fail.",
    LOGIN_SUCCESS: "Login success.",
    LOGOUT_SUCCESS: "Logout success.",
  };

  constructor(token: string | undefined) {
    this.is_prod = process.env.NODE_ENV === "production";
    this.client = mixpanel;
    if (token) {
      this.client.init(token, { debug: !this.is_prod });
    }
  }
}

export const Mixpanel = new MixpanelClient(token);
