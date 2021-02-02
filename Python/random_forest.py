Predictions = False
if Predictions:
	MHI = all_data['Median_12month_HH_Income']
	Pop = all_data['Population']
	SC = all_data['Service Connections']
	Renters = all_data['Median_rent_pct_Income']
	Reserve = all_data['cash_reserve_total']
	CES = all_data['CES 3.0 Score']
	# Reserve[Reserve == np.nan] = 0
	# print(Reserve.isna().sum())
	print(len(MHI))
	# Fee_Code = all_data['Fee Code']
	District = all_data['district']
	LPAs = []
	for i,x in enumerate(District):
		if x[0:3] == 'LPA':
			LPAs.append(1)
			District[i] = x[3:5]
		else:
			District[i] = x[9:11]
			LPAs.append(0)

	LPAs = pd.DataFrame(LPAs,columns=['LPAs'],index=MHI.index)
	print(LPAs)
	Assistance = all_data['months_before_assist']
	Assistance[Assistance == 'A'] = 3
	Assistance[Assistance == 'B'] = 6
	Assistance[Assistance == 'C'] = 9
	Assistance[Assistance == 'D'] = 12
	Assistance[Assistance == 'E'] = 15
	Assistance[Assistance == 'F'] = 20

	# print(len(Assistance))
	# print(len(MHI))
	# print(len(Pop))
	# print(len(SC))
	# print(len(District))

	from sklearn.ensemble import RandomForestClassifier
	from sklearn.preprocessing import StandardScaler
	scaler_X = StandardScaler()

	X = pd.concat([MHI,Pop,SC],axis=1)
	X = X.values
	X = scaler_X.fit_transform(X)
	Y = Assistance
	Y = Y.values

	Y = Y.reshape(-1,)
	Y = Y.astype('int')
	clsf = RandomForestClassifier(n_estimators=10000,random_state=3,max_depth=5)
	clsf = clsf.fit(X,Y)
	predictions = clsf.predict(X)

	correct = 0
	incorrect = 0
	for i,pred in enumerate(predictions):
		if pred == Y[i]:
			correct += 1
		else:
			incorrect += 1
	print('Correct',correct)
	print('Incorrect',incorrect)
	if correct+incorrect != len(Assistance):
		print('ISSUE!')
	print('Accuracy:',(correct/(correct+incorrect)))

	importances = clsf.feature_importances_
	print('Importances:',importances)