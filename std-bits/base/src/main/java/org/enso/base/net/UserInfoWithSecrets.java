package org.enso.base.net;

import org.enso.base.enso_cloud.HideableValue;

public record UserInfoWithSecrets(HideableValue username, HideableValue password) {}
